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
library(ggplot2)
library(ggpubr)
library(plotrix)
library(kableExtra)
theme_set(theme_pubr())

separate_by_four <-function(protein_names) {
 
  data1 = as.array(strsplit(prov_protein_names, ",")[[1]])

  protein_results = ""
  
  for(i in 1: nrow(data1))
    
    if(i%%4==0 & i<nrow(data1)) {
      
      data1[i]=paste0(data1[i],"\\n")
      
    }

  print(data1)
  
  return(paste(sort(data1 ), collapse = ", "))
  
}


tissue_column_name <- function(organs) {
  organs$Tissue_Initials = organs$Tissue.Specificity
  organs$Tissue_Initials = gsub(':','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub(',','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub(' ','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub('__','_',organs$Tissue_Initials)
  
  return(organs)
}

getwd()
setwd("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps")
getwd()

organs_mapping = read.csv(file = 'Organs_Olink_Insight_with_Initials.csv', sep = ';')
organs_mapping = tissue_column_name(organs_mapping)

organs_mapping$Gene[organs_mapping$Gene == "LGALS7, LGALS7B"] <- "LGALS7_LGALS7B"
organs_mapping$Gene[organs_mapping$Gene == "DEFA1, DEFA1B"] <- "DEFA1_DEFA1B"
organs_mapping$Gene[organs_mapping$Gene == "IL12A"] <- "IL12A_IL12B"




#Modelos
models = c("Deep","BR_MultinomialNB","BR_GaussianNB","ClassifierChains","LabelPowerSet",
           "RakelD","RandomForest","DecisionTree","MLkNN","HARAM","SVM", "BR_kNN")

split_factors = c("60_40","70_30","75_25")
#split_factors = c("60_40")

#mode = "Age_BMI"
mode = "Age_BMI"


df_tissues <- data.frame(matrix(ncol = 6, nrow = 0))
df_tissues_colnames <- c("Tissue","Total","Organ_Count","Tissue_Count","Split","Model")
colnames(df_tissues) <- df_tissues_colnames

df_proteins_all = data.frame()
#df_proteins_all_columns <- c('Gene', 'Organ')
#colnames(df_proteins_all) <- df_proteins_all_columns

for (split_factor_file in split_factors) {
  
  #Flattened CM Matrix Proteins
  for(model in models) {
    setwd(paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/',mode,'/ML Models/',model))
    getwd()
    if(file.exists(paste0('true_positive_proteins__flattened_',mode,'_',split_factor_file,'.csv'))) {
      df_proteins <- read_csv(paste0('true_positive_proteins__flattened_',mode,'_',split_factor_file,'.csv'))
      df_proteins <- df_proteins[-c(1)] #Drop index column
      colnames(df_proteins) <- c("Gene")
      df_proteins['Organ'] <- NA
      df_proteins$Organ <- organs_mapping[match(df_proteins$Gene, organs_mapping$Gene), 5]

      
      
      df_proteins['Organ_Count'] <- NA
      df_proteins['Organ_Count'] = 1:nrow(df_proteins)
      df_proteins['Tissue_Count'] <- NA
      df_proteins['Tissue_Count'] <- paste0(df_proteins$Organ_Count,"-",df_proteins$Organ)
      df_proteins['Model'] <- NA
      df_proteins['Model'] <- model
      df_proteins['Split'] <- split_factor_file
      df_proteins_all <- rbind(df_proteins_all,df_proteins)
      
      # Group by count using dplyr
      agg_tbl <- df_proteins %>% group_by(df_proteins$Organ) %>% 
        summarise(total_count=n(),
                  .groups = 'drop')
      
      colnames(agg_tbl) <- c("Tissue","Total")
      agg_tbl['Organ_Count'] <- NA
      agg_tbl['Organ_Count'] = 1:nrow(agg_tbl)
      agg_tbl['Tissue_Count'] <- NA
      agg_tbl['Tissue_Count'] <- paste0(agg_tbl$Organ_Count,"-",agg_tbl$Tissue)
      agg_tbl['Split'] <- NA
      agg_tbl['Split'] <- split_factor_file
      agg_tbl['Model'] <- NA
      agg_tbl['Model'] <- model
      
      agg_tbl$Split[agg_tbl$Split == "70_30"] <- "70%-30%"
      agg_tbl$Split[agg_tbl$Split == "60_40"] <- "60%-40%"
      agg_tbl$Split[agg_tbl$Split == "75_25"] <- "75%-25%"
      
      
      write.csv(agg_tbl,
                paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/',mode,'/ML Models/Results_Proteins_Tissues_',model,'_',split_factor_file,'.csv'),
                row.names = TRUE)
      
      
      #df_proteins_all <- rbind(df_proteins_all,df_proteins)
      
    }
    
    
    
    
    
  }
  
  
}

df_proteins_all = subset(df_proteins_all, select = -c(3,4))
df_proteins_all$Split[df_proteins_all$Split == "70_30"] <- "70%-30%"
df_proteins_all$Split[df_proteins_all$Split == "60_40"] <- "60%-40%"
df_proteins_all$Split[df_proteins_all$Split == "75_25"] <- "75%-25%"
df_proteins_all$Organ <- sub(".*: ", "", df_proteins_all$Organ) 
df_proteins_all$Organ <- sub(".*:", "", df_proteins_all$Organ) 

for (split_factor_file in split_factors) { 
  
  for(model in models) {
    
    
    if(file.exists(paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/',mode,'/ML Models/Results_Proteins_Tissues_',model,'_',split_factor_file,'.csv'))) {
      
      df <- read_csv(paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/',mode,'/ML Models/Results_Proteins_Tissues_',model,'_',split_factor_file,'.csv'))
      df_tissues <- rbind(df_tissues,df)
      
      
    }
    
    
  }
  
}

df_tissues$Tissue <- sub(".*: ", "", df_tissues$Tissue) 

df_tissues$Tissue <- sub(".*:", "", df_tissues$Tissue) 

write.csv(df_tissues,
          paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/',mode,'/ML Models/Results_Proteins_Tissues.csv'),
          row.names = TRUE)

df_tissues_ordered = df_tissues[order(df_tissues$Model,df_tissues$Split,-df_tissues$Total),]
unique_models = unique(df_tissues$Model)

split_factors = c("60%-40%","70%-30%","75%-25%")
df_plot <- data.frame(matrix(ncol = 4, nrow = 0))
df_plot_colnames <- c("Model","Split","Total1","Mode")
colnames(df_plot) <- df_plot_colnames

for(model in unique_models) {
  
  print(paste0("Model ->",model))
  #df <- agg_tbl$Split[agg_tbl$Split == "70_30"]
  df <- df_tissues[(df_tissues$Model == model),]
  
  for(split1 in split_factors) {
    print(paste0("Split ->",split1))
    total_partial <- sum(df$Total[(df$Split == split1)])
    total_tissues <- length(df$Tissue[which(df$Split == split1)])
    df_plot[nrow(df_plot)+1,] <- c(model,split1,total_partial,"Predicted potential biomarkers")
    df_plot[nrow(df_plot)+1,] <- c(model,split1,as.numeric(total_tissues),"Identified tissues")
  }
  
  
  
}

df_plot$Total1 = as.numeric(df_plot$Total1)

df_plot$Model[df_plot$Model == "RandomForest"] <- "RF"
df_plot$Model[df_plot$Model == "RakelD"] <- "RKD"
df_plot$Model[df_plot$Model == "LabelPowerSet"] <- "LPS SVM"
df_plot$Model[df_plot$Model == "Label Powerset GradientBooster"] <- "LPS GB"
df_plot$Model[df_plot$Model == "Deep"] <- "DP"
df_plot$Model[df_plot$Model == "DecisionTree"] <- "DT"
df_plot$Model[df_plot$Model == "ClassifierChains"] <- "CC"
df_plot$Model[df_plot$Model == "BR_MultinomialNB"] <- "BRV NB"
df_plot$Model[df_plot$Model == "BR_kNN"] <- "BRV kNN"
df_plot$Model[df_plot$Model == "BR_GaussianNB"] <- "LPS GB"

p <- ggballoonplot(df_plot, x = "Split", y = "Model", size = "Total1",
                   fill = "Total1", facet.by = "Mode",
                   ggtheme = theme_bw()) + guides(size = FALSE) + 
  scale_fill_viridis_c(option = "C") + geom_text(aes(label=Total1), alpha=1.0, size=2.5, nudge_x = 0, nudge_y =0.32)


ggpar(p,title = paste0("Proteins and tissues for ",ifelse(mode == "Age_BMI","E(B|A)","E(A|B)")),legend.title = "Values")


df_latex_table = data.frame(matrix(ncol=5, nrow=0))
df_latex_colnames = c("Split","Model","Tissue","Significant proteins","Ranking")
colnames(df_latex_table) = df_latex_colnames
split_factor_file_name = ""

for(split_factor_file in split_factors) {
  
  if(split_factor_file=="60%-40%") {
    split_factor_file_name = "60_40"
  }
  if(split_factor_file=="70%-30%") {
    split_factor_file_name = "70_30"
  }
  if(split_factor_file=="75%-25%") {
    split_factor_file_name = "75_25"
  }
  
  for (model in unique(df_proteins_all$Model)) {
    
    df_prov <- df_proteins_all[which(df_proteins_all$Split==split_factor_file & df_proteins_all$Model==model),]
    
    for(organ in unique(df_prov$Organ)) {
    
      
      prov_protein_names <- paste(sort(as.array(df_prov$Gene[df_prov$Organ == organ])), collapse = ", ")
    
     # prov_protein_names <- separate_by_four(prov_protein_names)
      
      df_latex_table[nrow(df_latex_table)+1,] = c(split_factor_file,model,organ,prov_protein_names, length(as.array(df_prov$Gene[df_prov$Organ == organ])))
    
      
      
    }
    #df_latex_table <- head(df_latex_table, n=10)
    
       # prov_protein_names <- paste0("\begin{tabular}[c]{@{}l@{}}",prov_protein_names,"\\end{tabular}")
    #df_latex_table$Ranking = as.numeric(df_latex_table$Ranking)
    #df_latex_table <- df_latex_table[order(df_latex_table$Ranking,decreasing=TRUE),]
  #  print(paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Age_BMI/ML Models/",model,"_latex_Age_BMI_",split_factor_file_name,".txt"))
  #  write.csv(df_latex_table,paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/",mode,"/ML Models/",model,"_Top10_proteins_tissue_",split_factor_file_name,"_flattened.csv"), row.names=TRUE)
    
     # df_latex_table <- head(df_latex_table, n=10)
  #  print(paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Age_BMI/ML Models/",model,"_latex_Age_BMI_",split_factor_file_name,".txt"))
  #  write.csv(df_latex_table,paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/",mode,"/ML Models/",model,"_Top10_proteins_tissue_",split_factor_file_name,"_flattened.csv"), row.names=TRUE)
  
    
    
      #latex_version = df_latex_table %>% kbl(caption = "Example", format = "latex", longtable = T) %>% kable_classic(html_font = "Cambria")
    
    
     #write(latex_version,paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/",mode,"/ML Models/latex_",model,"_Top10_proteins_tissue_",split_factor_file_name,"_flattened.tex"))
    
    
    
  }
 # df_latex_table$Ranking = as.numeric(df_latex_table$Ranking)
#  print(paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Age_BMI/ML Models/",model,"_latex_Age_BMI_",split_factor_file_name,".txt"))
#  write.csv(df_latex_table,paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/",mode,"/ML Models/",model,"_Top10_proteins_tissue_",split_factor_file_name,"_flattened.csv"), row.names=TRUE)
  
  
}

df_latex_table$Ranking = as.numeric(df_latex_table$Ranking)
df_latex_table_ordered = df_latex_table[with(df_latex_table, order(df_latex_table$Split, df_latex_table$Model,-df_latex_table$Ranking)), ]
write.csv(df_latex_table_ordered,paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/",mode,"/ML Models/Top10_proteins_tissue_flattened.csv"), row.names=TRUE)

### NAO FAZER EM FORMATO DE TABELA NO APENDICE




#latex_version = df_latex_table %>% kbl(caption = "Example", format = "latex", longtable = T) %>% kable_classic(html_font = "Cambria")
#print(latex_version)




#write(latex_version,paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/",mode,"/ML Models/latex_Top10_proteins_tissue_flattened.tex"))






#Select top 10 only
model_names = unique(df_tissues_ordered$Model)
df_plot_biomarkers_tissues = head(df_tissues_ordered,0)

for(split in split_factors)
{
  for (model in model_names) {
    #print(head(df_tissues_ordered[which(df_tissues_ordered$Model==model & df_tissues_ordered$Split==split),],10))
    df_plot_biomarkers_tissues  = rbind(df_plot_biomarkers_tissues ,head(df_tissues_ordered[which(df_tissues_ordered$Model==model & df_tissues_ordered$Split==split),],10))
  }
  
} 
df_plot_biomarkers_tissues = df_plot_biomarkers_tissues [-c(1,4,5)]
#df_plot_biomarkers_tissues ['SigProt'] <- "BMI (given Age)"
df_plot_biomarkers_tissues ['SigProt'] <- paste0(ifelse(mode == "Age_BMI","E(B|A)","E(A|B)"))


df_plot_biomarkers_tissues_aggregated <- aggregate(df_plot_biomarkers_tissues$Total, by=list(df_plot_biomarkers_tissues$SigProt,df_plot_biomarkers_tissues$Tissue,df_plot_biomarkers_tissues$Split,df_plot_biomarkers_tissues$Model), FUN=sum)
aggr_col_names <- c("SigProt","Tissue","Split","Model","Total")
colnames(df_plot_biomarkers_tissues_aggregated) = aggr_col_names

df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "RandomForest"] <- "RF"
df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "RakelD"] <- "RKD"
df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "LabelPowerSet"] <- "LPS SVM"
df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "Label Powerset GradientBooster"] <- "LPS GB"
df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "Deep"] <- "DP"
df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "DecisionTree"] <- "DT"
df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "ClassifierChains"] <- "CC"
df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "BR_MultinomialNB"] <- "BRV NB"
df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "BR_kNN"] <- "BRV kNN"
df_plot_biomarkers_tissues_aggregated$Model[df_plot_biomarkers_tissues_aggregated$Model == "BR_GaussianNB"] <- "LPS GB"


write.csv(df_plot_biomarkers_tissues_aggregated,paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Top10_proteins_tissue_flattened_",mode,".csv"), row.names=FALSE)


p<- ggballoonplot(
  df_plot_biomarkers_tissues_aggregated, x = "Model", y = "Tissue",
  size = "Total", fill = "Total",
  facet.by = c("Split","SigProt"), 
  ggtheme = theme_bw(base_size = 8) 
) +  guides(size = FALSE) + scale_fill_viridis_c(option = "C") + 
  geom_text(aes(label=Total), alpha=1.0, size=2.0, nudge_x = 0.35, nudge_y =0) 




plot(ggpar(p,title = paste0("Potential biomarkers - ", ifelse(mode == "Age_BMI","E(B|A)","E(A|B)"))))


source("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Elevated Proteins/Results_Proteins_Tissues_BMI_Age.R")