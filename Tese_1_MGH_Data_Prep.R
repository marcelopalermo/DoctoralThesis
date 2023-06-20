#install.packages("remotes")
# remotes::install_github(repo ='Olink-Proteomics/OlinkRPackage/OlinkAnalyze', ref = "main", build_vignettes = TRUE)

#install.packages("OlinkAnalyze")
#remove.packages("OlinkAnalyze")
# open package
library(OlinkAnalyze)
#install.packages("dplyr")
library(dplyr)

library(xml2)
library(tidyverse)
library(stringr)
library(data.table)
library(umap)
#install.packages("ggplotify")
library(ggplotify)
#install.packages("pheatmap")
library(pheatmap)

tissue_column_name <- function(organs) {
  organs$Tissue_Initials = organs$Tissue.Specificity
  organs$Tissue_Initials = gsub(':','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub(',','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub(' ','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub('__','_',organs$Tissue_Initials)
  
  return(organs)
}

results_when_exception <- function() {
  
  df_results_age_missing <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("B", "Bname", "Type", "Measure","A","Aname","Results"))
  
  
  for(i in 1:5) { #Age
    for(j in 0:4) { #BMI
      Age_label = switch(i,"20-34yrs","35-49yrs","50-64yrs","65-79yrs","80+yrs")
      BMI_label = switch (j+1,"0-underweight","1-normal","2-overweight","3-obese","4-severely obese")
      
      if(n_distinct(which(df_results_age_split$B==j & df_results_age_split$A==i & df_results_age_split$Measure=="ANOVA" & df_results_age_split$Type=="1-Training"))==0) {
        print(paste0(i,"-",j))
         
         print(paste0("Adding training data exception results for BMI = ",BMI_label," and Age = ",Age_label))
         df_results_age_missing[nrow(df_results_age_missing)+1,] = c(j,BMI_label,"1-Training","ANOVA",i,Age_label,0)
         
         
         ##CORRIGIR - tem uns que tem em Training mas não em Testing e vice-versa
        
      }
      
      if(n_distinct(which(df_results_age_split$B==j & df_results_age_split$A==i & df_results_age_split$Measure=="ANOVA" & df_results_age_split$Type=="2-Testing"))==0) {
        print(paste0(i,"-",j))
        
        print(paste0("Adding testing data exception results for BMI = ",BMI_label," and Age = ",Age_label))
        df_results_age_missing[nrow(df_results_age_missing)+1,] = c(j,BMI_label,"2-Testing","ANOVA",i,Age_label,0)
      }
        
      
    }
  }
  print(nrow(df_results_age_missing))
  print(df_results_age_missing)

  return(df_results_age_missing)
  
  
}


getwd()
setwd("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Elevated Proteins")
getwd()

proteins = as.data.frame(read.csv('Protein_Elevation_Charts_TV_zeroed.csv'))


getwd()
setwd("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps")
getwd()


#Add Organs to Proteins
organs_mapping = read.csv(file = 'Organs_Olink_Insight_with_Initials.csv', sep = ';')
organs_mapping = tissue_column_name(organs_mapping)

#---------------------------------------------------------------------------#
#---------CREATING THE NECESSARY NPX AND ANOVA FILES -----------------------#
#-----------JUST FOR THE RELEVANT COMBINATIONS------------------------------#
#---------------------------------------------------------------------------#

directory = paste0(getwd(),"/AGE_BMI_Significatives/")

Mode_TV = ""
#Split_Factor = "65_35"
Split_Factor = split_factor_file

# ORIGINAL Ages = unique(df_Age_to_BMI[,1])

# ORIGINAL for (i in 1:length(Ages)) 
for (i in 1:5) {
  print(i)
  csv_file_train = paste0('MGH_NPX_dataframe_age_categories_train_',i,'.csv')
  NPX_dataframe_age_categories_train = read.csv(file = csv_file_train)
  #Drop Column X - Useless data
  NPX_dataframe_age_categories_train = subset(NPX_dataframe_age_categories_train, select = -c(X) )
  csv_file_validation = paste0('MGH_NPX_dataframe_age_categories_validation_',i,'.csv')
  NPX_dataframe_age_categories_validation = read.csv(file = csv_file_validation)
  #Drop Column X - Useless data
  NPX_dataframe_age_categories_validation = subset(NPX_dataframe_age_categories_validation, select = -c(X) )
  #  NPX_dataframe_age_categories = read.csv(file = paste0('MGH_NPX_dataframe_age_categories_',Age,'.csv'))
  print(paste0("Reading... ", csv_file_train, " - ",csv_file_validation))
  
  for(j in 0:4) {
    tryCatch({
      # ORIGINAL: for(j in 1:length(df_Age_to_BMI$BMI[which(df_Age_to_BMI$Age==i)])) {
      # ORIGINAL: BMI_j = df_Age_to_BMI$BMI[which(df_Age_to_BMI$Age==i)][j]
      
      
      #-------TRAIN-----------#
      Mode_TV = "Train"
      print(paste0("ANOVA for ",csv_file_train, " for BMI=",j))
      anova = olink_anova(NPX_dataframe_age_categories_train[which(NPX_dataframe_age_categories_train$BMI_cat==j),], variable = "Timepoint")
      #Cut off Non-Significant values
      anova <- anova[which(anova$Threshold == 'Significant'), ]
      print(paste0(">>>> ", Mode_TV," - ANOVA number of significant training rows = ",nrow(anova)," for ",csv_file_validation, " for BMI=",j))
      
      
      
      anova <- anova[order(anova$Adjusted_pval,decreasing = FALSE),]
      
      
      
      anova['Organ'] <- NA
      anova$Organ <- organs_mapping[match(anova$UniProt, organs_mapping$UniProt.ID), 5]
      
      anova['Desc'] <- NA
      anova$Desc <- organs_mapping[match(anova$UniProt, organs_mapping$UniProt.ID), 3]
      
      anova['Desc_Column'] <- NA
      anova$Desc_Column <- organs_mapping[match(anova$UniProt, organs_mapping$UniProt.ID), 6]
      
      
      #Write CSV OlinkANOVA results
      print(paste0(directory, " - Writing.... MGH_ANOVA_BMI_categories_train_Age_",i,"_BMI_",j,".csv"))
      write.csv(anova,
                paste0(directory, "MGH_ANOVA_BMI_categories_train_Age_",i,"_BMI_",j,".csv"),
                row.names = TRUE)
      
      if (nrow(anova)>20) {
        anova_cut <- head(anova,20)
        write.csv(anova_cut,
                  paste0(directory, "MGH_ANOVA_BMI_categories_train_Age_",i,"_BMI_",j,"_cut.csv"),
                  row.names = TRUE)
        
      }
      
      
      Age_label = switch(i,"20-34yrs","35-49yrs","50-64yrs","65-79yrs","80+yrs")
      BMI_label = switch (j+1,"0-underweight","1-normal","2-overweight","3-obese","4-severely obese")
      print(paste0("Age_label for ", Mode_TV, " = ",Age_label, ", BMI_label = ",BMI_label))
      proteins$Significant.Protein.elevation[proteins$Age == Age_label & proteins$BMI == BMI_label & proteins$Mode==Mode_TV] = nrow(anova)
      
      
      df_results_age_split[nrow(df_results_age_split)+1,] = c(j,BMI_label,"1-Training","ANOVA",i,Age_label,nrow(anova))

      
      
      #NPX_patients = N1[which(N1$subject_id == patients[indice_patient] & N1$subject_id == patients[indice_patient+1]),]

      #-------VALIDATION-----------#
      Mode_TV="Validation"
      print(paste0("ANOVA for ",csv_file_validation, " for BMI=",j))
      anova = olink_anova(NPX_dataframe_age_categories_validation[which(NPX_dataframe_age_categories_validation$BMI_cat==j),], variable = "Timepoint")
      #Cut off Non-Significant values
      anova <- anova[which(anova$Threshold == 'Significant'), ]
      print(paste0(">>>> ", Mode_TV," - ANOVA number of significant validation rows = ",nrow(anova)," for ",csv_file_validation, " for BMI=",j))
      
      anova <- anova[order(anova$Adjusted_pval,decreasing = FALSE),]
      
      
      anova['Organ'] <- NA
      anova$Organ <- organs_mapping[match(anova$UniProt, organs_mapping$UniProt.ID), 5]
      
      anova['Desc'] <- NA
      anova$Desc <- organs_mapping[match(anova$UniProt, organs_mapping$UniProt.ID), 3]
      
      anova['Desc_Column'] <- NA
      anova$Desc_Column <- organs_mapping[match(anova$UniProt, organs_mapping$UniProt.ID), 6]
      
      
      #Write CSV OlinkANOVA results
      print(paste0(directory, " - Writing.... MGH_ANOVA_BMI_categories_validation_Age_",i,"_BMI_",j,".csv"))
      
      write.csv(anova,
                paste0(directory, "MGH_ANOVA_BMI_categories_validation_Age_",i,"_BMI_",j,".csv"),
                row.names = TRUE)
      
      if (nrow(anova)>20) {
        anova_cut <- head(anova,20)
        write.csv(anova_cut,
                  paste0(directory, "MGH_ANOVA_BMI_categories_validation_Age_",i,"_BMI_",j,"_cut.csv"),
                  row.names = TRUE)
        
      }
      
      
      
      
      Age_label = switch(i,"20-34yrs","35-49yrs","50-64yrs","65-79yrs","80+yrs")
      BMI_label = switch (j+1,"0-underweight","1-normal","2-overweight","3-obese","4-severely obese")
      print(paste0("Age_label for ", Mode_TV, " = ",Age_label, ", BMI_label = ",BMI_label))
      proteins$Significant.Protein.elevation[proteins$Age == Age_label & proteins$BMI == BMI_label & proteins$Mode==Mode_TV] = nrow(anova)
      
      df_results_age_split[nrow(df_results_age_split)+1,] = c(j,BMI_label,"2-Testing","ANOVA",i,Age_label,nrow(anova))
      
      

    }, error=function(e) {
      print(e)
      
      print(paste0("Age=",i," + BMI = ",j," on ",Mode_TV, " has no significant ANOVA elevated proteins"))
      
      
    }
    
    )
  }
  
  write.csv(proteins,
            paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Elevated Proteins/Protein_Elevation_Charts_Age_BMI_",Split_Factor,".csv"),
            row.names = TRUE)
  
}

df_results_age_miss = results_when_exception()

df_results_age_split = rbind(df_results_age_split,df_results_age_miss)


write.csv(df_results_age_split,
          paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Results_Age_BMI_",split_factor_file,".csv"),
          row.names = TRUE)
