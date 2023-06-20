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

tissue_column_name <- function(organs) {
  organs$Tissue_Initials = organs$Tissue.Specificity
  organs$Tissue_Initials = gsub(':','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub(',','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub(' ','_',organs$Tissue_Initials)
  organs$Tissue_Initials = gsub('__','_',organs$Tissue_Initials)
  
  return(organs)
}

mode = "BMI_Age"


biomarkers = read.csv(file = '/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/BMI_Age/ML Models/Deep/true_positive_proteins__flattened_BMI_Age_75_25.csv', sep = ',')

organs_mapping = read.csv(file = '/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Organs_Olink_Insight.csv', sep = ';')
organs_mapping = tissue_column_name(organs_mapping)

organs_mapping$Gene[organs_mapping$Gene == "LGALS7, LGALS7B"] <- "LGALS7_LGALS7B"
organs_mapping$Gene[organs_mapping$Gene == "DEFA1, DEFA1B"] <- "DEFA1_DEFA1B"
organs_mapping$Gene[organs_mapping$Gene == "IL12A, IL12B"] <- "IL12A_IL12B"

biomarkers['Organ'] <- NA
biomarkers$Organ <- organs_mapping[match(biomarkers$X0, organs_mapping$Gene), 5]

