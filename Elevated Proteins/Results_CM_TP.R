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


setwd(paste0('/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Results/'))
    getwd()
df_TP_CM <- read_csv(paste0('Results_CM_TP.csv'))

p <- ggballoonplot(df_TP_CM, x = "Split", y = "Model", size = "TP",
                   fill = "TP", facet.by = "Sorting",
                   ggtheme = theme_bw()) + guides(size = FALSE) + 
  scale_fill_viridis_c(option = "C") + geom_text(aes(label=TP), alpha=1.0, size=2.5, nudge_x = 0.38, nudge_y =0)


plot(ggpar(p,title = paste0("Predicted biomarkers (TP) from Confusion Matrices"),legend.title = "Biomarkers"))
