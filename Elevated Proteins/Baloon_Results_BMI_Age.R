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
theme_set(theme_pubr())

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


# Grouped frequency table
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dframe1 <- read.csv(file = paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/Results_BMI_Age_',split_factor_file,'.csv'))

p <- ggballoonplot(
  dframe1, x = "Aname", y = "Measure",
  size = "Results", fill = "Results",
  facet.by = c("Bname","Type"), 
  ggtheme = theme_bw()
) + guides(size = FALSE) + scale_fill_viridis_c(option = "C") + 
  geom_text(aes(label=dframe1$Results), alpha=1.0, size=3.5, nudge_x = 0, nudge_y =0.45)

plot(ggpar(p,title = paste0("Train: ",str_split(split_factor_file,"_")[[1]][1]," %, Testing: ",str_split(split_factor_file,"_")[[1]][2],"%"),legend.title = "Number of\nelevated proteins"))


#scale_fill_gradientn(colors = my_cols)


#colSums(dframe1[which(dframe1$Type=="1-Training" & dframe1$Measure=="NPX"),]['Results'])
#colSums(dframe1[which(dframe1$Type=="2-Testing" & dframe1$Measure=="NPX"),]['Results'])
#colSums(dframe1[which(dframe1$Measure=="NPX"),]['Results'])