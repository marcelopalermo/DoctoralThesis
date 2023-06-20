library(stringr)

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())



getwd()
setwd("/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/Elevated Proteins")
getwd()

#proteins = read.csv('Protein_Elevation_Charts_BMI_Age_65_35.csv')
proteins = read.csv(paste0('Protein_Elevation_Charts_BMI_Age_',split_factor_file,'.csv'))


df <- as.data.frame(proteins)
head(df)

p <- ggballoonplot(df, x = "Age", y = "BMI", size = "Significant.Protein.elevation",
              fill = "Significant.Protein.elevation", facet.by = "Mode",
              ggtheme = theme_bw()) + guides(size = FALSE) + 
  scale_fill_viridis_c(option = "C") + geom_text(aes(label=df$Significant.Protein.elevation), alpha=1.0, size=2.5, nudge_x = 0, nudge_y =0.42)


ggpar(p,title = paste0("Train: ",str_split(split_factor_file,"_")[[1]][1]," %, Testing: ",str_split(split_factor_file,"_")[[1]][2],"%"),legend.title = "Number of\nelevated proteins")

#when running via batch (source())
plot(ggpar(p,title = paste0("Train: ",str_split(split_factor_file,"_")[[1]][1]," %, Testing: ",str_split(split_factor_file,"_")[[1]][2],"%"),legend.title = "Number of\nelevated proteins"))



p<- ggballoonplot(df, x = "Age", y = "BMI", 
              size = "Significant.Protein.elevation", fill = "Significant.Protein.elevation") + scale_fill_viridis_c(option = "C")
ggpar(p,legend.title = "Elevated proteins")
#ggpar(p,caption = "BMI",legend.title = "Protein elevation")


p<- ggballoonplot(df, x = "Age", y = "BMI", 
                  size = "Significant.Protein.elevation", fill = "Significant.Protein.elevation") +
                  scale_fill_viridis_c(option = "C") +
                  guides(size = FALSE) + labs(title="Number of COVID-19 elevated proteins per BMI based on Age")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))
ggpar(p,legend.title = "Number of\nelevated proteins")

