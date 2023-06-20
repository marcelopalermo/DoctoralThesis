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
df_CPU_Time <- read_csv(paste0('Results_CPU_Time.csv'))

p <- ggballoonplot(df_CPU_Time, x = "Split", y = "Model", size = "Time",
                   fill = "Time", facet.by = "Sorting",
                   ggtheme = theme_bw()) + guides(size = FALSE) + 
  scale_fill_viridis_c(option = "C") + geom_text(aes(label=Time), alpha=1.0, size=2.5, nudge_x = 0.32, nudge_y =0)


plot(ggpar(p,title = paste0("CPU Time elapsed per ML Model (in seconds)"),legend.title = "Seconds"))
