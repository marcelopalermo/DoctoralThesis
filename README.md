
Versions used:
- R 4.3.0 and R Studio latest version 2023.06.0+421
- Python 3.10
- Jupyter Notebook 6.5.4 on Anaconda 3


1. Download the Massachusetts General Hospital COVID patients’ cohort from https://olink.com/application/mgh-covid-19-study/
2. Open R or R studio and install the package OlinkAnalyze via install.packages(“OlinkAnalyze”)
3. Copy the file MGH_COVID_OLINK_NPX.csv to the R.Framework OlinkAnalyze lidrary extdata directory, example:

/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library/OlinkAnalyze/extdata

4. Download the entire content from https://github.com/marcelopalermo/DoctoralThesis into a directory of your preference. Mine is: Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/
5. Open Tese_1.MGH.R in R Studio and replace all “/Users/marcelo_palermo/Doutorado/R/Olink/NextSteps/“ occurrences to match your directory structure downloaded on step 4
6. Still on Tese_1_MGH.R, set the 3 occurrences of split_factor = 0.60 (for 60%-40% split strategy), or split_factor = 0.70 (for 70%-30% split strategy), or split_factor = 0.75 (for 75%-25% split strategy)
7. Run Tese_1.MGH.R to generate all ANOVA calculations and R data preparations according to split strategies from item 4
8. Start Jupyter Notebook
9. Open and edit Tese_MGH_Datah_Cleansing_Train_*_NOCUT.ipynb file and replace split_factor_file_train and split_factor_file_validation to “60_40” (for 60%-40% split strategy), or “70_30” (for 70%-30% split strategy), or “75_25” (for 75%-25% split strategy)
10. Run Tese_MGH_Datah_Cleansing_Train_*_NOCUT.ipynb to cleanse the data and prepare for the ML model operations. One file will produce sorting Age to BMI and the other will produce the sorting BMI to Age.
11. Open and edit Tese_MGH_FINAL2_Train_*_Models_proteins_NOCUT.ipynb, by repeating the changes from step 9
12. Run Tese_MGH_FINAL2_Train_*_Models_proteins_NOCUT.ipynb to run all ML models (but Deep Learning approach) for Age to BMI sorting, and for BMI to Age sorting
13. Open and edit Tese_MGH_FINAL2_Deep_Data Cleansing_Train-*_train_proteins_NOCUT.ipynb and repeat step 9
14. Run Tese_MGH_FINAL2_Deep_Data Cleansing_Train-*_train_proteins_NOCUT.ipynb to run the deep learning model
15. All results will be stored on Results subdirectory under the downloaded structure from step 4
