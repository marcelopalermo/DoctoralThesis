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
#theme_set(theme_pubr())
theme_set(theme_classic2())

# 
# # Grouped frequency table
# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# data("Titanic")
# dframe <- as.data.frame(Titanic)
# head(dframe)
# #>   Class    Sex   Age Survived Freq
# #> 1   1st   Male Child       No    0
# #> 2   2nd   Male Child       No    0
# #> 3   3rd   Male Child       No   35
# #> 4  Crew   Male Child       No    0
# #> 5   1st Female Child       No    0
# #> 6   2nd Female Child       No    0
# ggballoonplot(
#   dframe, x = "Class", y = "Sex",
#   size = "Freq", fill = "Freq",
#   facet.by = c("Survived", "Age"),
#   ggtheme = theme_bw()
# ) + guides(size = FALSE) + scale_fill_viridis_c(option = "C") #scale_fill_gradientn(colors = my_cols)
# 



library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())


getwd()
setwd("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results")
getwd()



# Grouped frequency table
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::


df_metrics <- data.frame()

df1 <- read.csv(file = paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Age_BMI/ML Models/Scores_Age_BMI_60_40.csv'))
df_metrics <- rbind(df_metrics,df1)
df1 <- read.csv(file = paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Age_BMI/ML Models/Scores_Age_BMI_70_30.csv'))
df_metrics <- rbind(df_metrics,df1)
df1 <- read.csv(file = paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Age_BMI/ML Models/Scores_Age_BMI_75_25.csv'))
df_metrics <- rbind(df_metrics,df1)
df_metrics$X = "BMI (given Age)"


df1 <- read.csv(file = paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/BMI_Age/ML Models/Scores_BMI_Age_60_40.csv'))
df_metrics <- rbind(df_metrics,df1)
df1 <- read.csv(file = paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/BMI_Age/ML Models/Scores_BMI_Age_70_30.csv'))
df_metrics <- rbind(df_metrics,df1)
df1 <- read.csv(file = paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/BMI_Age/ML Models/Scores_BMI_Age_75_25.csv'))
df_metrics <- rbind(df_metrics,df1)
df_metrics$X[df_metrics$X!="BMI (given Age)"] = "Age (given BMI)"



df_metrics$Proportion[df_metrics$Proportion == "70_30"] <- "70%-30%"
df_metrics$Proportion[df_metrics$Proportion == "60_40"] <- "60%-40%"
df_metrics$Proportion[df_metrics$Proportion == "75_25"] <- "75%-25%"

df_metrics$Model[df_metrics$Model == "RandomForest"] <- "RF"
df_metrics$Model[df_metrics$Model == "RakelD"] <- "RKD"
df_metrics$Model[df_metrics$Model == "Label Powerset SVC"] <- "LPS SVM"
df_metrics$Model[df_metrics$Model == "Label Powerset GradientBooster"] <- "LPS GB"
df_metrics$Model[df_metrics$Model == "Deep"] <- "DP"
df_metrics$Model[df_metrics$Model == "DecisionTree"] <- "DT"
df_metrics$Model[df_metrics$Model == "ClassifierChain"] <- "CC"
df_metrics$Model[df_metrics$Model == "BR_MultinomialNB"] <- "BRV NB"
df_metrics$Model[df_metrics$Model == "BR_kNN"] <- "BRV kNN"







df_metrics_baloon_plot = data.frame(matrix(ncol = 5, nrow = 0))
c <- c('Bname','Type','Measure','Aname','Results')
colnames(df_metrics_baloon_plot) = c

label = 1

for(i in 1:nrow(df_metrics)) {
  row1 = df_metrics[i,]
  print(paste(row1))
  print("--------")
  for(results_row in 4:8) {
    
    metrics_label = switch(results_row - 3,"1-Hamming Loss","2-Precision","3-Recall","4-F1 Score","5-Jaccard")  
    print(metrics_label)
    
    df_metrics_baloon_plot[nrow(df_metrics_baloon_plot) + 1,] <- 
      list(metrics_label,df_metrics[i,1],df_metrics[i,2],df_metrics[i,3], round(df_metrics[i,results_row], digits = 3))
    
    
    
  }
  
  
  
  
}  




p<- ggballoonplot(
  df_metrics_baloon_plot, x = "Bname", y = "Measure",
  size = 6, fill = "Results",
  facet.by = c("Aname","Type"), 
  ggtheme = theme_bw()
) + guides(size = FALSE) + gradient_fill(c("lightyellow", "orange", "palevioletred1")) + #
 # scale_fill_gradientn(colours = cm.colors(20)) +
  geom_text(aes(label=Results, fontface="bold"), alpha=1, size=3.5, nudge_x = -0.37, nudge_y =0)

plot(ggpar(p, legend.title = "Index values"))



