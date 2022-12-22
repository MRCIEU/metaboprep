############################################################
## Paramater file for Nightingale & Metabolon QC Processing
## 			by: Laura Corbin and David Hughes
## 			date: May 15th 2019
############################################################
## (1) Name of your project
projectname=MYPROJECTNAME
## (2) Full path to the directory holding your data file(s)
datadirectory=/FULL/PATH/TO/DATA/DIRECTORY/
## (3) Metabolite or Protien data file name
##				- Could be:
##				  1. Commercially provided excel sheet from Metabolon or Nightingale. 
## 				  2. Or a flat text file (csv or txt) with metabolites in columns, samples in rows
##					- With metabolite names on the first row and sample names on the first column.
metabolite_data_file=TheNameOfMyComercial_Metabolon_ExcelSheet.xls
## (4) feature (metabolite) annotation file
## 				- ** NOT ** necessary if reading in excel file provided by Metabolon or Nightingale
## 				- features in row, annotation in columns
## 				- NOTE: Metabolon QC requires a "SUPER_PATHWAY" column header to identify Xenobiotics
##				- NOTE: DO NOT provide a feature annotation file if you have Nightingale Health Data
##				-       we have an annotation table ng_anno in metaboprep that adds feature annotation
##				-		- We are working to allow for the Nightingale Health data release format that includes an annotation tab/file.
feature_annotation_file=NA
## (5) sample annotation file (batch variables)
## 				- ** NOT ** necessary if reading in excel file provided by Metabolon or Nightingale
## 				- samples in rows, annotation in columns
sample_annotation_file=NA
## (6) Is your dataset from Metabolon, Nightingale, or Other
Nightingale_OR_Metabolon=Metabolon
## (7) FEATURE QC: Proportion of missinginess on features used as a QC threshold. 
feature_missingness=0.2
## (8) SAMPLE QC: Proportion of missinginess on samples used as a QC threshold. 
sample_missingness=0.2
## (9) SAMPLE QC: The number of standard deviation (SD) units from the mean to perform a sample QC based on total abundance
##				- If you would like to exclude total sum abundance as a QC parameter set the value to NA.
total_peak_area_SD=5
## (10) PCA Prep: Outlier values for features. The number of interquartile range (IQR) unit distances from the median to call a value an outlier
##				- "outliers" here are extreme values that are the product of error.
outlier_udist=5
## (11) PCA Prep: What to do with outliers for the purposes of the PCA, and for the PCA only.
##				- set to "leave_be" if you would like no action on outliers
##				- set to "winsorize" if you would like outliers to be winsorized to the 100th quantile of all remaining (non-outying) values, at a feature.
##				- set to "turn_NA" if you would like outliers converted to NA. This means they will be imputed to the median for the purposes of the PCA.
outlier_treatment=leave_be
## (12) PCA Prep: Feature|metabolite independence:
##				- to identify "independent" features in your data set, how similar should clustered|grouped features be?
##				- tree_cut_height = 1-absolute(spearman's rho)
##				- such that a tree_cut_height of 0.2 would indicate a intra-cluster similarity of >0.8, and a tree_cut_height of 0.8 would indicate a intra-cluster similarity of >0.2.
##				- larger tree_cut_height values yield more clustering and thus fewer "representative" or "independent" features.
tree_cut_height=0.5
## (13) SAMPLE QC: The number of standard deviation (SD) units from the mean to perform a sample QC based on principal components
##				- If you would like to exclude PC exclusions as a QC parameter set the value to NA.
##				- Setting to NA may be advisable if you expect significant structure among individuals for example your data is derived from differnt tissues, different geographies/ecologies/environments
PC_outlier_SD=5
## (14) Nightingale derived variable exclusion
##				- derived variables in Nightingale are all variables derived from two or more variables already present
##				- ... in the data set. In this instance it represents all ratios that Nightingale supply in their data releaases.
##				- binary choice of TRUE or FALSE
derived_var_exclusion=TRUE
## (15) 'Feature Annotation File' column name holding the run mode identifier for each metabolite.
##				- The string(s) in this column should match a column name in the 
##				- 'Sample Annotation File' that hold the batch IDs for that run mode. 
##				- This is commonly needed data for MS data.
##				- NA, should be given if run mode column name not known, or unavailable, or inapprorpriate.
feat_anno_run_mode_col=NA
## (16) Plot feature distributions and summary statistics, for each metabolite | protein in the data set to a pdf.
plot_feature_distributions=TRUE
