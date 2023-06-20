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

get_comments <- function(UniProt_ID) {
  print(UniProt_ID)
  xml_address = paste("https://rest.uniprot.org/uniprotkb/",
                      UniProt_ID,
                      ".xml",
                      sep = "")
  proteins_xml = as_list(read_xml(xml_address))
  
  xml_df = tibble::as_tibble(proteins_xml) %>%
    unnest_longer(uniprot)
  
  lp_wider = xml_df %>%
    #   dplyr::filter(uniprot_id == "comment") %>%
    dplyr::filter(uniprot_id == "protein") %>%
    unnest_wider(uniprot)
  
  lp_df = lp_wider %>%
    # 1st time unnest to release the 2-dimension list?
    #    unnest(cols = names(.)) %>%
    # 2nd time to nest the single list in each cell?
    #    unnest(cols = names(.)) %>%
    # convert data type
    readr::type_convert()
  
  #Sys.sleep(1)
  return(lp_df$recommendedName[[1]]$fullName[[1]])
  #return(lp_df$text[[1]][[1]])
}

protein_description <- function(anova_results) {

  for (i in 1:nrow(anova_results))
  {
    
    #Processar somente os que não tem "_". Depois fazer uma rotina no final desta (nova função) só para pegar os que tem "_" para evitar for dentro de for Big(On2) 
    if (grepl("_", anova_results$UniProt[i]) == FALSE) {
      print(anova_results$Assay[i])
      protein_comment = get_comments(anova_results$UniProt[i])
      anova_results$Desc[i] = protein_comment
      print(protein_comment)
    }

  } 
  
  return (anova_results)
  
}

protein_description_multiple <- function(anova_results) {
  
  protein_desc = NULL
  double_proteins_dataframe = anova_results[anova_results$UniProt %like% "_",]
  list_proteins = double_proteins_dataframe$UniProt
  protein_uniprot_code = str_split(list_proteins,pattern = "_",simplify = TRUE)
  print(protein_uniprot_code)
  for (i in 1:length(list_proteins)) {
    protein_comment = get_comments(protein_uniprot_code[i,1])
    protein_desc = protein_comment
    protein_comment = get_comments(protein_uniprot_code[i,2])
    protein_desc = paste(protein_desc,"+",protein_comment)
    
    anova_results$Desc[anova_results$UniProt == list_proteins[i]] = protein_desc
  }
  
  return (anova_results)
}

train_validation_MGH <- function(NPX_data, split_factor,shuffle) {
  
  if(shuffle == TRUE) {
    rows <- sample(nrow(NPX_data))
    NPX_data <- NPX_data[rows,]
    NPX_data <- NPX_data[order(NPX_data$subject_id),]
  }
  

  
  #ordenar
  #NPX_dataframe_old <- NPX_dataframe_old[order(NPX_dataframe_old$subject_id),]
  
  subject_id_set = unique(NPX_data$subject_id)
  print(subject_id_set)
  print(length(subject_id_set))
  
  
  #TRAINING SET
  start <- 1
  end <- as.integer(round(length(subject_id_set) * split_factor))
  patients_training <- as.vector(subject_id_set[start:end])
  print(paste(start,end))
  print(patients_training)
  NPX_data_train <- NPX_data[NPX_data$subject_id %in% patients_training,]
  
  #VALIDATION SET
  start <- end + 1
  end <- length(subject_id_set)
  patients_validation<- as.vector(subject_id_set[start:end])
  print(paste(start,end))
  print(patients_validation)
  NPX_data_validation <- NPX_data[NPX_data$subject_id %in% patients_validation,]
  
  
  
  return(list(NPX_data_train, NPX_data_validation))
  
}

train_validation_new <- function(NPX_data, split_factor,shuffle) {
  
  #REMOVE Unknown BMI = 5
  NPX_data = NPX_data[which(NPX_data$BMI_cat != 5 ),]
  
  
  if(shuffle == TRUE) {
    rows <- sample(nrow(NPX_data))
    NPX_data <- NPX_data[rows,]
    NPX_data <- NPX_data[order(NPX_data$SampleID),]
  } 

  # #NÃO RANDOMICO
  # SampleID_set = unique(NPX_data$SampleID)
  # print(SampleID_set)
  # print(length(SampleID_set))
  
  
  #RANDOMICO
  print("-------RANDOM---------")
  SampleID_set = sample(unique(NPX_data$SampleID))
  print(SampleID_set)
  print(length(SampleID_set))
  
  
  
  #TRAINING
  #Patients_A
  start <- 1
  end <- as.integer(round(length(SampleID_set) / 2 * split_factor))
  Patients_A <- as.vector(SampleID_set[start:end])
  print(Patients_A)
  
  #Patients_B
  start <- as.integer(length(SampleID_set) / 2 + 1)
  end <-
    as.integer(round(length(SampleID_set) / 2 * split_factor) + length(SampleID_set) -
                 round(length(SampleID_set) / 2))
  Patients_B <-  as.vector(SampleID_set[start:end])
  print(Patients_B)
  
  #TRAINING DATA
  NPX_data_train <-
    rbind(NPX_data[NPX_data$SampleID %in% Patients_A, ], NPX_data[NPX_data$SampleID %in% Patients_B, ])
  
  #TEST
  #Patients_A
  start <-
    as.integer(round(length(SampleID_set) / 2 * split_factor) + 1)
  end <- as.integer(round(length(SampleID_set) / 2))
  Patients_A <- as.vector(SampleID_set[start:end])
  print(Patients_A)
  
  #Patients_B
  start <-
    as.integer(round(length(SampleID_set) / 2 * split_factor) + length(SampleID_set) -
                 round(length(SampleID_set) / 2) + 1)
  end <- length(SampleID_set)
  Patients_B <-  as.vector(SampleID_set[start:end])
  print(Patients_B)
  
  #TRAINING DATA
  NPX_data_validation <-
    rbind(NPX_data[NPX_data$SampleID %in% Patients_A, ], NPX_data[NPX_data$SampleID %in% Patients_B, ])
  
  return(list(NPX_data_train, NPX_data_validation))
  
}

lot.NPX <- function(x,
                    labels,
                    text,
                    main = text,
                    colors = c("#ff7f00", "#e377c2", "#17becf"),
                    pad = 0.1,
                    cex = 0.9,
                    pch = 19,
                    add = FALSE,
                    legend.suffix = "",
                    cex.main = 1.5,
                    cex.legend = 1) {
  layout = x
  if (is(x, "umap")) {
    layout = x$layout
  }
  
  xylim = range(layout)
  xylim = xylim + ((xylim[2] - xylim[1]) * pad) * c(-0.5, 0.5)
  if (!add) {
    par(mar = c(0.2, 0.7, 1.2, 0.7), ps = 10)
    plot(xylim,
         xylim,
         type = "n",
         axes = F,
         frame = F)
    rect(xylim[1],
         xylim[1],
         xylim[2],
         xylim[2],
         border = "#aaaaaa",
         lwd = 0.25)
  }
  points(layout[, 1],
         layout[, 2],
         col = colors[as.integer(labels)],
         cex = cex,
         pch = pch)
  mtext(side = 3, main, cex = cex.main)
  
  labels.u = unique(labels)
  legend.pos = "topright"
  legend.text = as.character(labels.u)
  if (add) {
    legend.pos = "bottomright"
    legend.text = paste(as.character(labels.u), legend.suffix)
  }
  legend(
    legend.pos,
    legend = legend.text,
    col = colors[as.integer(labels.u)],
    bty = "n",
    pch = pch,
    cex = cex.legend
  )
}

tissue_column_name <- function(organs) {
  organs$Tissue_Initials = organs$Tissue.Specificity
  organs$Tissue_Initials = gsub(':','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub(',','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub(' ','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub('__','_',organs$Tissue_Initials)
  
  return(organs)
}

ANOVA_each_patient <- function(DataframePatient) {
  
  anova_results_patient <- olink_anova(DataframePatient, variable = 'Timepoint')
  
  anova_results_patient <-
    anova_results_patient[which(anova_results_old$Threshold == 'Significant'), ]
  
}


getwd()
setwd("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps")
getwd()

# keep same random sequence
set.seed(500)


# 4.2 /Library/Frameworks/R.framework/Versions/4.2/Resources/library/OlinkAnalyze/extdata
# COPIAR O ARQUIVO: exdata vem do caminho /Users/marcelo_palermo/Library/R/x86_64/4.1/library/OlinkAnalyze/extdata
#file <- system.file("extdata", "Example_NPX_Data.csv", package = "OlinkAnalyze")


# Load Massachussets General Hospital COVID Data - 386 patients

file <-
 # system.file("extdata", "All_Proteins_NPX_new.csv", package = "OlinkAnalyze")
  system.file("extdata", "MGH_COVID_OLINK_NPX.csv", package = "OlinkAnalyze")

# Este é o diretório onde fica o arquivo
print(file)
NPX_csv <- read_NPX(file)
#print(NPX_csv)
NPX_dataframe <- as.data.frame(NPX_csv)

print("If I decided to remove non_D7 patients (D3 and DE means early discharge or death) I would be left with only 36% of patients")
print("So I decided to include them all to preserve the nature of the experience")

#unique(subset(NPX_dataframe$SampleID, grepl('D7', NPX_dataframe$SampleID)))
print(length(unique(subset(NPX_dataframe$SampleID, grepl('D7', NPX_dataframe$SampleID))))/length(unique(subset(NPX_dataframe$SampleID, grepl('D0', NPX_dataframe$SampleID)))))




#Add Clinical Info
MGH_clinical_info = read.csv(file = 'MGH_COVID_Clinical_Info.csv')

#----------------Add Clinical Info to patients NPX_dataframe
`%notlike%` <- Negate(`%like%`)

NPX_Control <- NPX_dataframe[NPX_dataframe$SampleID %notlike% "_D", ]


print(paste0("Control Patients = ",length(unique(NPX_Control$SampleID))))

#Cut control patients
NPX_dataframe <- NPX_dataframe[NPX_dataframe$SampleID %like% "_D", ]
nrow(NPX_dataframe)




#Eliminate patients with one measurement, only 
# 1) ANOVA won't work for one measurement, only
# 2) Patient may have died during the measurements
# 3) We are interested in many measurements to detect the real longitudinal protein expression
#table(NPX_dataframe_old$subject_id)==1472

#COVID-Positive-Negative
NPX_dataframe['COVID'] <- NA
NPX_dataframe$COVID <- MGH_clinical_info[match(NPX_dataframe$subject_id, MGH_clinical_info$subject_id),2]



#Cut off COVID-negative testing
NPX_COVID_Negative <- NPX_dataframe[NPX_dataframe$COVID %like% "0", ]
print(paste0("COVID-Negative Patients excluded = ", length(unique(NPX_COVID_Negative$subject_id))))

NPX_dataframe <- NPX_dataframe[NPX_dataframe$COVID %like% "1", ]
print(paste0("COVID-Positive Patients = ", length(unique(NPX_dataframe$subject_id))))



#Age range
NPX_dataframe['Age_cat'] <- NA
NPX_dataframe$Age_cat <- MGH_clinical_info[match(NPX_dataframe$subject_id, MGH_clinical_info$subject_id),3]

#BMI range
NPX_dataframe['BMI_cat'] <- NA
NPX_dataframe$BMI_cat <- MGH_clinical_info[match(NPX_dataframe$subject_id, MGH_clinical_info$subject_id),4]


#Age range for Thesis - Multilabel



anova_results_total = olink_anova(NPX_dataframe,variable = 'Timepoint')

write.csv(NPX_dataframe,
          "MGH_NPX_dataframe_initial.csv",
          row.names = TRUE) 

write.csv(anova_results_total,
          "MGH_ANOVA__total.csv",
          row.names = TRUE) 


summary(NPX_dataframe)




#----------------------------------------------------------------------AGE
# Separate patients per Age Label
NPX_dataframe_old <- NPX_dataframe[which(NPX_dataframe$Age_cat > 2),]
write.csv(NPX_dataframe_old,
          "MGH_NPX_dataframe_old.csv",
          row.names = TRUE)

anova_results_old <- olink_anova(NPX_dataframe_old, variable = 'Timepoint')

anova_results_old <-
  anova_results_old[which(anova_results_old$Threshold == 'Significant'), ]

write.csv(anova_results_old,
          "MGH_ANOVA_results_old.csv",
          row.names = TRUE)


NPX_dataframe_young <- NPX_dataframe[which(NPX_dataframe$Age <= 2),]
write.csv(NPX_dataframe_young,
          "MGH_NPX_dataframe_young.csv",
          row.names = TRUE)

anova_results_young <- olink_anova(NPX_dataframe_young, variable = 'Timepoint')

anova_results_young <-
  anova_results_young[which(anova_results_young$Threshold == 'Significant'), ]

write.csv(anova_results_young,
          "MGH_ANOVA_results_young.csv",
          row.names = TRUE)

split_factor = 0.60
split_factor_file = paste0(split_factor*100,"_",(1-split_factor)*100)
shuffle = FALSE
NPX_train_validation = train_validation_MGH(NPX_dataframe_old,split_factor,shuffle)
NPX_dataframe_old_train = as.data.frame(NPX_train_validation[1])
NPX_dataframe_old_validation = as.data.frame(NPX_train_validation[2])


write.csv(NPX_dataframe_old_train,
          "MGH_NPX_dataframe_old_train.csv",
          row.names = TRUE)

write.csv(NPX_dataframe_old_validation,
          "MGH_NPX_dataframe_old_validation.csv",
          row.names = TRUE)


#FUNCTION: #Sequential or random choice of patients for train and validation
#YOUNG PATIENTS
NPX_train_validation = train_validation_new(NPX_dataframe_young,split_factor,shuffle)

NPX_dataframe_young_train = as.data.frame(NPX_train_validation[1])
NPX_dataframe_young_validation = as.data.frame(NPX_train_validation[2])


write.csv(NPX_dataframe_young_train,
          "MGH_NPX_dataframe_young_train.csv",
          row.names = TRUE)

write.csv(NPX_dataframe_young_validation,
          "MGH_NPX_dataframe_young_validation.csv",
          row.names = TRUE)


#-------------------------------------------------------------------------------
##OlinkAnalyze ANOVA calculations
#-------------------------------------------------------------------------------

#----------------------------------------------------------------------------OLD
#ANOVA Training Data
anova_results_old_train <-
  olink_anova(df = NPX_dataframe_old_train, variable = 'Timepoint')
#Cut off non-significant data
anova_results_old_train <-
  anova_results_old_train[which(anova_results_old_train$Threshold == 'Significant'), ]
#Add label to identify OLD patients
anova_results_old_train = anova_results_old_train %>% mutate(label_desc = '50 above',label = 1)

#ANOVA Validation data
anova_results_old_validation <-
  olink_anova(df = NPX_dataframe_old_validation, variable = 'Timepoint')
#Cut off non-significant data
anova_results_old_validation <-
  anova_results_old_validation[which(anova_results_old_validation$Threshold == 'Significant'), ]

#Add label to identify OLD patients
anova_results_old_validation = anova_results_old_validation %>% mutate(label_desc = '50 above',label = 1)



#--------------------------------------------------------------------------YOUNG
#ANOVA Training Data
anova_results_young_train <-
  olink_anova(df = NPX_dataframe_young_train, variable = 'Timepoint')
#Cut off non-significant data
anova_results_young_train <-
  anova_results_young_train[which(anova_results_young_train$Threshold == 'Significant'), ]

#Add label to identify young patients
anova_results_young_train = anova_results_young_train %>% mutate(label_desc = '49 below',label = 0)


#ANOVA Validation data
anova_results_young_validation <-
  olink_anova(df = NPX_dataframe_young_validation, variable = 'Timepoint')
#Cut off non-significant data
anova_results_young_validation <-
  anova_results_young_validation[which(anova_results_young_validation$Threshold == 'Significant'), ]

#Add label to identify young patients
anova_results_young_validation = anova_results_young_validation %>% mutate(label_desc = '49 below',label = 0)


#-------------------------------------------------------------------------------
#------------------------------------JOIN OLD AND YOUNG FOR TRAIN AND VALIDATION

anova_results_age_train = rbind(anova_results_old_train,anova_results_young_train)
anova_results_age_validation = rbind(anova_results_old_validation,anova_results_young_validation)

#Add Organs to Proteins
organs_mapping = read.csv(file = 'Organs_Olink_Insight_with_Initials.csv', sep = ';')
organs_mapping = tissue_column_name(organs_mapping)

anova_results_age_train['Organ'] <- NA
anova_results_age_train$Organ <- organs_mapping[match(anova_results_age_train$UniProt, organs_mapping$UniProt.ID), 5]

anova_results_age_validation['Organ'] <- NA
anova_results_age_validation$Organ <- organs_mapping[match(anova_results_age_validation$UniProt, organs_mapping$UniProt.ID), 5]

anova_results_age_train['Desc'] <- NA
anova_results_age_train$Desc <- organs_mapping[match(anova_results_age_train$UniProt, organs_mapping$UniProt.ID), 3]

anova_results_age_train['Desc_Column'] <- NA
anova_results_age_train$Desc_Column <- organs_mapping[match(anova_results_age_train$UniProt, organs_mapping$UniProt.ID), 6]


anova_results_age_validation['Desc'] <- NA
anova_results_age_validation$Desc <- organs_mapping[match(anova_results_age_validation$UniProt, organs_mapping$UniProt.ID), 3]

anova_results_age_validation['Desc_Column'] <- NA
anova_results_age_validation$Desc_Column <- organs_mapping[match(anova_results_age_validation$UniProt, organs_mapping$UniProt.ID), 6]



write.csv(anova_results_age_train,
          "MGH_ANOVA_results_age_train.csv",
          row.names = TRUE)

write.csv(anova_results_age_validation,
          "MGH_ANOVA_results_age_validation.csv",
          row.names = TRUE)

olink_umap_plot(df=NPX_dataframe_young, color_g = "Timepoint")                       

olink_umap_plot(df=NPX_dataframe_old[!(NPX_dataframe_old$Timepoint =="DE" | NPX_dataframe_old$Timepoint =="DE.1" | NPX_dataframe_old$Timepoint =="DE.2" | NPX_dataframe_old$Timepoint =="D3"),], color_g = "Timepoint")

olink_umap_plot(df=NPX_dataframe_young[!(NPX_dataframe_young$Timepoint =="DE" | NPX_dataframe_young$Timepoint =="DE.1" | NPX_dataframe_young$Timepoint =="DE.2" | NPX_dataframe_young$Timepoint =="D3"),], color_g = "Timepoint")


#RESULTS MATRIX
#df_results_age_split <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("B", "Bname", "Type", "Measure","A","Aname","Results"))

columns = c("B", "Bname", "Type", "Measure","A","Aname","Results")
df_results_age_split <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_results_age_split) = columns



#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#-----------DATA EXPLORATION AND THESIS PREPARATION ------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------SEPARATE AGE PER 5 CATEGORIES, AS PER MGH DATASET-----------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#


#To handle each categoy, use as.data.frame(NPX_dataframe_age_categories[1])
NPX_dataframe_age_categories = split(NPX_dataframe, f = NPX_dataframe$Age_cat)
volcano_lists_age = list()
umap_lists_age = list()
heatmap_lists_age = list()

#Age category    1 = 20-34    2 = 35-49    3 = 50-64    4 = 65-79    5 = 80+
#All ANOVA Trainings dataframe - cumulative
#anova_results_age_category_training_all = list()
#All ANOVA Validation dataframe - cumulative
#anova_results_age_category_validation_all = list()


for(i in 1:length(NPX_dataframe_age_categories)) {
  print(paste("Analyzing Age Category:",i))
  #Write NPX data as CSV per category (for sequencing & debug purposes)
  write.csv(NPX_dataframe_age_categories[i],
            paste0("MGH_NPX_dataframe_age_categories_",i,".csv"),
            row.names = TRUE)
  
  #Calculate OlinkANOVA per category (for sequencing & debug purposes)
  print(paste("ANOVA per Entire Age Category (debug purposes):",i))
  anova_results_age_category = olink_anova(as.data.frame(NPX_dataframe_age_categories[[i]]), variable = "Timepoint")
  
  #Cut off Non-Significant values
  anova_results_age_category <-
    anova_results_age_category[which(anova_results_age_category$Threshold == 'Significant'), ]
  
  #Add Human Organs related to Proteins
  #organs_mapping = read.csv(file = 'Organs_Table.csv')
  organs_mapping = read.csv(file = 'Organs_Olink_Insight_with_Initials.csv', sep = ';')
  organs_mapping = tissue_column_name(organs_mapping)
  
  anova_results_age_category['Organ'] <- NA
  #anova_results_age_category$Organ <- organs_mapping[match(anova_results_age_category$Assay, organs_mapping$PlasmaProtein), 2]
  anova_results_age_category$Organ <- organs_mapping[match(anova_results_age_category$UniProt, organs_mapping$UniProt.ID), 5]
  
  
  #Add Protein Descriptions
  #protein_desc = read.csv(file = 'Protein_names_final.csv')
  anova_results_age_category['Desc'] <- NA
  #anova_results_age_category$Desc <- protein_desc[match(anova_results_age_category$Assay, protein_desc$Assay), 4]
  anova_results_age_category$Desc <- organs_mapping[match(anova_results_age_category$UniProt, organs_mapping$UniProt.ID), 3]
  
  anova_results_age_category['Desc_Column'] <- NA
  anova_results_age_category$Desc_Column <- organs_mapping[match(anova_results_age_category$UniProt, organs_mapping$UniProt.ID), 6]
  
  
  
  #Write CSV OlinkANOVA results
  write.csv(anova_results_age_category,
          paste0("MGH_ANOVA_age_categories_",i,".csv"),
          row.names = TRUE)
  
  #UMAP from AGE Category
  print(paste0("olink_umap - Age_cat = ",unique(NPX_dataframe_age_categories[[i]]$Age_cat)))
  NPX_UMAP = as.data.frame(NPX_dataframe_age_categories[[i]])
  #g <- olink_umap_plot(df=NPX_UMAP[!(NPX_UMAP$Timepoint =="DE" | NPX_UMAP$Timepoint =="DE.1" | NPX_UMAP$Timepoint =="DE.2" | NPX_UMAP$Timepoint =="D3"),], color_g = "Timepoint")
  g <- olink_umap_plot(df=NPX_UMAP, color_g = "Timepoint")
  #olink_umap_plot(df=NPX_UMAP, color_g = "Timepoint", label_samples = TRUE)
  umap_lists_age[[length(umap_lists_age)+1]] <- g
  
  #olink_heatmap_plot(df=NPX_UMAP, fontsize = 0.5)
  #olink_qc_plot(NPX_UMAP, color_g = "Timepoint")
  
  print(paste0("###### Volcano Plot - Age_cat = ",unique(NPX_dataframe_age_categories[[i]]$Age_cat)))
  ttest_results <- olink_ttest(df=NPX_UMAP[!(NPX_UMAP$Timepoint =="DE" | NPX_UMAP$Timepoint =="DE.1" | NPX_UMAP$Timepoint =="DE.2" | NPX_UMAP$Timepoint =="D3"),],
                               variable = 'Timepoint',
                               alternative = 'two.sided')
 # ttest_results <- ttest_results[which(ttest_results$Threshold == 'Significant'), ]
  g <-olink_volcano_plot(ttest_results)
  volcano_lists_age[[length(volcano_lists_age)+1]] <- g
  
  library(dplyr)
  npx_data <- NPX_UMAP
  
  #Heatmap with annotation
  h <- olink_heatmap_plot(df=npx_data, variable_row_list = c('Timepoint'))
  heatmap_lists_age[[length(heatmap_lists_age)+1]] <- h
  
  
  #---------------------------------------------------------------------------#
  #SEPARATE AGE DATASET AS TRAIN AND VALIDATION                               #
  #---------------------------------------------------------------------------#
  split_factor = 0.60
  shuffle = FALSE
  
  print(paste("Split Training Data: split_factor:",split_factor))
  NPX_train_validation = train_validation_new(NPX_dataframe_age_categories[[i]],split_factor,shuffle)
  
  #NPX_dataframe_age_categories_train = as.data.frame(NPX_train_validation[1])
  #NPX_dataframe_age_categories_validation = as.data.frame(NPX_train_validation[2])
  

  
  ### RESULTS HERE (FOR NPX)
  for(BMI in 0:4){
    Age_label = switch(i,"20-34yrs","35-49yrs","50-64yrs","65-79yrs","80+yrs")
    BMI_label = switch (BMI+1,"0-underweight","1-normal","2-overweight","3-obese","4-severely obese")
    df_results_age_split[nrow(df_results_age_split)+1,] = c(BMI,BMI_label,"1-Training","NPX",i,Age_label,n_distinct(NPX_train_validation[[1]][which(NPX_train_validation[[1]]$BMI_cat == BMI),]))
    df_results_age_split[nrow(df_results_age_split)+1,] = c(BMI,BMI_label,"2-Testing","NPX",i,Age_label,n_distinct(NPX_train_validation[[2]][which(NPX_train_validation[[2]]$BMI_cat == BMI),]))
    
  }

  
  write.csv(as.data.frame(NPX_train_validation[1]),
            paste0("MGH_NPX_dataframe_age_categories_train_",i,".csv"),
            row.names = TRUE)
  
  write.csv(as.data.frame(NPX_train_validation[2]),
            paste0("MGH_NPX_dataframe_age_categories_validation_",i,".csv"),
            row.names = TRUE)
  
  #ANOVA Training Data
  print(paste0("ANOVA Training Data Age for category ",i))
  anova_results_age_category <-
    olink_anova(df = as.data.frame(NPX_train_validation[1]), variable = 'Timepoint')
  
  #Cut off Non-Significant values
  anova_results_age_category <-
    anova_results_age_category[which(anova_results_age_category$Threshold == 'Significant'), ]
  
  #Add Human Organs related to Proteins
  #organs_mapping = read.csv(file = 'Organs_Table.csv')
  anova_results_age_category['Organ'] <- NA
  #anova_results_age_category$Organ <- organs_mapping[match(anova_results_age_category$Assay, organs_mapping$PlasmaProtein), 2]
  anova_results_age_category$Organ <- organs_mapping[match(anova_results_age_category$UniProt, organs_mapping$UniProt.ID), 5]
  
  
  #Add Protein Descriptions
  #protein_desc = read.csv(file = 'Protein_names_final.csv')
  anova_results_age_category['Desc'] <- NA
  #anova_results_age_category$Desc <- protein_desc[match(anova_results_age_category$Assay, protein_desc$Assay), 4]
  anova_results_age_category$Desc <- organs_mapping[match(anova_results_age_category$UniProt, organs_mapping$UniProt.ID), 3]
  
  anova_results_age_category['Desc_Column'] <- NA
  anova_results_age_category$Desc_Column <- organs_mapping[match(anova_results_age_category$UniProt, organs_mapping$UniProt.ID), 6]
  
  
  
  #Write CSV OlinkANOVA results
  write.csv(anova_results_age_category,
            paste0("MGH_ANOVA_age_categories_train_",i,".csv"),
            row.names = TRUE)
  #anova_results_age_category_training_all = rbind(anova_results_age_category_training_all,anova_results_age_category)
  
  #ANOVA Validation Data
  print(paste0("ANOVA Validation Data for Age category ",i))
  anova_results_age_category <-
    olink_anova(df = as.data.frame(NPX_train_validation[2]), variable = 'Timepoint')
  
  #Cut off Non-Significant values
  anova_results_age_category <-
    anova_results_age_category[which(anova_results_age_category$Threshold == 'Significant'), ]
  
  
  #Add Human Organs related to Proteins
  #organs_mapping = read.csv(file = 'Organs_Table.csv')
  anova_results_age_category['Organ'] <- NA
  #anova_results_age_category$Organ <- organs_mapping[match(anova_results_age_category$Assay, organs_mapping$PlasmaProtein), 2]
  anova_results_age_category$Organ <- organs_mapping[match(anova_results_age_category$UniProt, organs_mapping$UniProt.ID), 5]
  
  
  #Add Protein Descriptions
  #protein_desc = read.csv(file = 'Protein_names_final.csv')
  anova_results_age_category['Desc'] <- NA
  #anova_results_age_category$Desc <- protein_desc[match(anova_results_age_category$Assay, protein_desc$Assay), 4]
  anova_results_age_category$Desc <- organs_mapping[match(anova_results_age_category$UniProt, organs_mapping$UniProt.ID), 3]
  
  anova_results_age_category['Desc_Column'] <- NA
  anova_results_age_category$Desc_Column <- organs_mapping[match(anova_results_age_category$UniProt, organs_mapping$UniProt.ID), 6]
  
  #Write CSV OlinkANOVA results
  write.csv(anova_results_age_category,
            paste0("MGH_ANOVA_age_categories_validation_",i,".csv"),
            row.names = TRUE)
  
 # anova_results_age_category_validation_all = rbind(anova_results_age_category_validation_all,anova_results_age_category)
  

}

print("umap plots stored and available in umap_lists_age[i]:")
print("volcano plots stored and available in volcano_lists_age[[i]]:")
print("i in Age category    1 = 20-34    2 = 35-49    3 = 50-64    4 = 65-79    5 = 80+")


write.csv(df_results_age_split,
          paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Results_Age_BMI_",split_factor_file,".csv"),
          row.names = TRUE)



#RESULTS MATRIX BMI
#df_results_age_split <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("B", "Bname", "Type", "Measure","A","Aname","Results"))

columns = c("B", "Bname", "Type", "Measure","A","Aname","Results")
df_results_BMI_split <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_results_BMI_split) = columns

#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------SEPARATE BMI PER 4 CATEGORIES, AS PER MGH DATASET-----------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#To handle each categoy, use as.data.frame(NPX_dataframe_age_categories[1])
NPX_dataframe_BMI_categories = split(NPX_dataframe, f = NPX_dataframe$BMI_cat)

#Body mass index:    0 = <18.5 (underweight)    1 = 18.5-24.9 (normal)    2 = 25.0-29.9 (overweight)    3 = 30.0-39.9 (obese)    4 = >=40 (severely obese)    5 = Unknown
#All ANOVA Trainings dataframe - cumulative
#anova_results_BMI_category_training_all = list()
#All ANOVA Validation dataframe - cumulative
#anova_results_BMI_category_validation_all = list()


#Cut off BMI_cat=5 (Unknown)
NPX_dataframe_BMI_categories <- NPX_dataframe_BMI_categories[-6]
volcano_lists_BMI = list()
umap_lists_BMI = list()

for(i in 1:length(NPX_dataframe_BMI_categories)) {
  tryCatch({
  print(paste("Analyzing BMI Category:",i-1))
  #Write NPX data as CSV per category (for sequencing & debug purposes)
  write.csv(NPX_dataframe_BMI_categories[i],
            paste0("MGH_NPX_dataframe_BMI_categories_",i-1,".csv"),
            row.names = TRUE)
  
  #Calculate OlinkANOVA per category (for sequencing & debug purposes)
  print(paste("ANOVA per Entire BMI Category (debug purposes):",i-1))
  anova_results_BMI_category = olink_anova(as.data.frame(NPX_dataframe_BMI_categories[[i]]), variable = "Timepoint")
  
  #Cut off Non-Significant values
  anova_results_BMI_category <-
    anova_results_BMI_category[which(anova_results_BMI_category$Threshold == 'Significant'), ]
  
  print(paste0("ANOVA nrow = ",nrow(anova_results_BMI_category)))
  
  #Add Human Organs related to Proteins
  #organs_mapping = read.csv(file = 'Organs_Table.csv')
  anova_results_BMI_category['Organ'] <- NA
  #anova_results_BMI_category$Organ <- organs_mapping[match(anova_results_BMI_category$Assay, organs_mapping$PlasmaProtein), 2]
  anova_results_BMI_category$Organ <- organs_mapping[match(anova_results_BMI_category$UniProt, organs_mapping$UniProt.ID), 5]
  
  
  
  #Add Protein Descriptions
  #protein_desc = read.csv(file = 'Protein_names_final.csv')
  anova_results_BMI_category['Desc'] <- NA
  #anova_results_BMI_category$Desc <- protein_desc[match(anova_results_BMI_category$Assay, protein_desc$Assay), 4]
  anova_results_BMI_category$Desc <- organs_mapping[match(anova_results_BMI_category$UniProt, organs_mapping$UniProt.ID), 3]
  
  anova_results_BMI_category['Desc_Column'] <- NA
  anova_results_BMI_category$Desc_Column <- organs_mapping[match(anova_results_BMI_category$UniProt, organs_mapping$UniProt.ID), 6]
  
  
  
  
  #Write CSV OlinkANOVA results
  write.csv(anova_results_BMI_category,
            paste0("MGH_ANOVA_BMI_categories_",i-1,".csv"),
            row.names = TRUE)
  #anova_results_BMI_category_training_all = rbind(anova_results_BMI_category_training_all,anova_results_BMI_category)
  
  tryCatch({
  #UMAP from BMI Category
  print(paste0("olink_umap - BMI_cat = ",unique(NPX_dataframe_BMI_categories[[i]]$BMI_cat)))
  NPX_UMAP = as.data.frame(NPX_dataframe_BMI_categories[[i]])
  #g <- olink_umap_plot(df=NPX_UMAP[!(NPX_UMAP$Timepoint =="DE" | NPX_UMAP$Timepoint =="DE.1" | NPX_UMAP$Timepoint =="DE.2" | NPX_UMAP$Timepoint =="D3"),], color_g = "Timepoint")
  g <- olink_umap_plot(df=NPX_UMAP, color_g = "Timepoint")
  umap_lists_BMI[[length(umap_lists_BMI)+1]] <- g
  print(paste0("##### Volcano Plot - BMI_cat = ",unique(NPX_dataframe_BMI_categories[[i]]$BMI_cat)))
  
  }, error=function(e){
    print(e)
  
    })
  
  
  ttest_results <- olink_ttest(df=NPX_UMAP[!(NPX_UMAP$Timepoint =="DE" | NPX_UMAP$Timepoint =="DE.1" | NPX_UMAP$Timepoint =="DE.2" | NPX_UMAP$Timepoint =="D3"),],
                               variable = 'Timepoint',
                               alternative = 'two.sided')
  # ttest_results <- ttest_results[which(ttest_results$Threshold == 'Significant'), ]
  
  g <-olink_volcano_plot(ttest_results)
  volcano_lists_BMI[[length(volcano_lists_BMI)+1]] <- g
  
  
  #---------------------------------------------------------------------------#
  #SEPARATE BMI DATASET AS TRAIN AND VALIDATION                               #
  #---------------------------------------------------------------------------#
  split_factor = 0.60
  shuffle = FALSE
  
  print(paste("Split Training Data: split_factor:",split_factor))
  NPX_train_validation = train_validation_new(NPX_dataframe_BMI_categories[[i]],split_factor,shuffle)
  
  ### RESULTS HERE (FOR NPX)
  for(BMI_Age in 1:5){
    Age_label = switch(BMI_Age,"20-34yrs","35-49yrs","50-64yrs","65-79yrs","80+yrs")
    BMI_label = switch (i,"0-underweight","1-normal","2-overweight","3-obese","4-severely obese")
    df_results_BMI_split[nrow(df_results_BMI_split)+1,] = c(i-1,BMI_label,"1-Training","NPX",BMI_Age,Age_label,n_distinct(NPX_train_validation[[1]][which(NPX_train_validation[[1]]$Age_cat == BMI_Age),]))
    df_results_BMI_split[nrow(df_results_BMI_split)+1,] = c(i-1,BMI_label,"2-Testing","NPX",BMI_Age,Age_label,n_distinct(NPX_train_validation[[2]][which(NPX_train_validation[[2]]$Age_cat == BMI_Age),]))
    
  }
  
  
  write.csv(as.data.frame(NPX_train_validation[1]),
            paste0("MGH_NPX_dataframe_BMI_categories_train_",i-1,".csv"),
            row.names = TRUE)
 
  
  write.csv(as.data.frame(NPX_train_validation[2]),
            paste0("MGH_NPX_dataframe_BMI_categories_validation_",i-1,".csv"),
            row.names = TRUE)
  
  #ANOVA Training Data
  print(paste0("ANOVA Training Data BMI for category ",i-1))
  anova_results_BMI_category <-
    olink_anova(df = as.data.frame(NPX_train_validation[1]), variable = 'Timepoint')
  
  #Cut off Non-Significant values
  anova_results_BMI_category <-
    anova_results_BMI_category[which(anova_results_BMI_category$Threshold == 'Significant'), ]
  
  
  #Add Human Organs related to Proteins
  #organs_mapping = read.csv(file = 'Organs_Table.csv')
  anova_results_BMI_category['Organ'] <- NA
  #anova_results_age_category$Organ <- organs_mapping[match(anova_results_age_category$Assay, organs_mapping$PlasmaProtein), 2]
  anova_results_BMI_category$Organ <- organs_mapping[match(anova_results_BMI_category$UniProt, organs_mapping$UniProt.ID), 5]
  
  
  #Add Protein Descriptions
  #protein_desc = read.csv(file = 'Protein_names_final.csv')
  anova_results_BMI_category['Desc'] <- NA
  #anova_results_age_category$Desc <- protein_desc[match(anova_results_age_category$Assay, protein_desc$Assay), 4]
  anova_results_BMI_category$Desc <- organs_mapping[match(anova_results_BMI_category$UniProt, organs_mapping$UniProt.ID), 3]
  
  anova_results_BMI_category['Desc_Column'] <- NA
  anova_results_BMI_category$Desc_Column <- organs_mapping[match(anova_results_BMI_category$UniProt, organs_mapping$UniProt.ID), 6]
  
  
  
  #Write CSV OlinkANOVA results
  write.csv(anova_results_BMI_category,
            paste0("MGH_ANOVA_BMI_categories_train_",i-1,".csv"),
            row.names = TRUE)
  
  #ANOVA Validation Data
  print(paste0("ANOVA Validation Data for BMI category ",i-1))
  anova_results_BMI_category <-
    olink_anova(df = as.data.frame(NPX_train_validation[2]), variable = 'Timepoint')
  
  #Cut off Non-Significant values
  anova_results_BMI_category <-
    anova_results_BMI_category[which(anova_results_BMI_category$Threshold == 'Significant'), ]
  
  
  #Add Human Organs related to Proteins
  #organs_mapping = read.csv(file = 'Organs_Table.csv')
  anova_results_BMI_category['Organ'] <- NA
  #anova_results_age_category$Organ <- organs_mapping[match(anova_results_age_category$Assay, organs_mapping$PlasmaProtein), 2]
  anova_results_BMI_category$Organ <- organs_mapping[match(anova_results_BMI_category$UniProt, organs_mapping$UniProt.ID), 5]
  
  
  #Add Protein Descriptions
  #protein_desc = read.csv(file = 'Protein_names_final.csv')
  anova_results_BMI_category['Desc'] <- NA
  #anova_results_age_category$Desc <- protein_desc[match(anova_results_age_category$Assay, protein_desc$Assay), 4]
  anova_results_BMI_category$Desc <- organs_mapping[match(anova_results_BMI_category$UniProt, organs_mapping$UniProt.ID), 3]
  
  anova_results_BMI_category['Desc_Column'] <- NA
  anova_results_BMI_category$Desc_Column <- organs_mapping[match(anova_results_BMI_category$UniProt, organs_mapping$UniProt.ID), 6]
  
  
  
  #Write CSV OlinkANOVA results
  write.csv(anova_results_BMI_category,
            paste0("MGH_ANOVA_BMI_categories_validation_",i-1,".csv"),
            row.names = TRUE)
  #anova_results_BMI_category_validation_all = rbind(anova_results_BMI_category_validation_all,anova_results_BMI_category)
  
  
  }, error=function(e){print(e)})
   
  
}



#write.csv(anova_results_BMI_category_training_all,
#          paste0("MGH_ANOVA_BMI_categories_training_all.csv"),
#          row.names = TRUE)

#write.csv(anova_results_BMI_category_validation_all,
#          paste0("MGH_ANOVA_BMI_categories_validation_all.csv"),
#          row.names = TRUE)

print("umap plots stored and available in umap_lists_BMI[i]:")
print("volcano plots stored and available in volcano_lists_BMI[[i]]:")
print("i in BMI category   Body mass index:    1: =<18.5 (underweight) (No volcano plot)   2: =18.5-24.9 (normal)    3: =25.0-29.9 (overweight)    4: = 30.0-39.9 (obese)    5: >=40 (severely obese) ---> 6:Unknown is excluded")

write.csv(df_results_BMI_split,
          paste0("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Results_BMI_Age_",split_factor_file,".csv"),
          row.names = TRUE)

#split_factor_file = paste0(split_factor*100,"_",(1-split_factor)*100)


source("Tese_1_MGH_Data_Prep.R")
source("Tese_1_MGH_Data_Prep_BMI_Age.R")
source("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Elevated Proteins/Baloon_Plot_AGE_vs_BMI.R")
source("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Elevated Proteins/Baloon_Plot_BMI_vs_Age.R")
#source("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Elevated Proteins/Baloon_Results_Age_BMI.R")
