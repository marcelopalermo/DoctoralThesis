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

mode = "BMI_Age"
setwd(paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/',mode,'/ML Models/'))



biomarkers = read.csv(file = 'Top10_proteins_tissue_flattened.csv', sep = ',')
biomarkers = biomarkers[-c(1)]

biomarkers$Model[biomarkers$Model == "RandomForest"] <- "RF"
biomarkers$Model[biomarkers$Model == "RakelD"] <- "RKD"
biomarkers$Model[biomarkers$Model == "LabelPowerSet"] <- "LPS SVM"
biomarkers$Model[biomarkers$Model == "Label Powerset GradientBooster"] <- "LPS GB"
biomarkers$Model[biomarkers$Model == "Deep"] <- "DP"
biomarkers$Model[biomarkers$Model == "DecisionTree"] <- "DT"
biomarkers$Model[biomarkers$Model == "ClassifierChains"] <- "CC"
biomarkers$Model[biomarkers$Model == "BR_MultinomialNB"] <- "BRV NB"
biomarkers$Model[biomarkers$Model == "BR_kNN"] <- "BRV kNN"
biomarkers$Model[biomarkers$Model == "BR_GaussianNB"] <- "LPS GB"

tex_string = "\\begin{itemize}"


for(i in 1:nrow(biomarkers)) {
  
  tex_string = paste(tex_string,"#item \\textbf{Model:", biomarkers[i,2],"- Split:",biomarkers[i,1], "-", biomarkers[i,3], "-", biomarkers[i,5], "potential biomarker(s):}\\\\",biomarkers[i,4])  
  
  
}



tex_string = paste(tex_string," #end{itemize}")




write(tex_string, "appendixC_biomarkers")