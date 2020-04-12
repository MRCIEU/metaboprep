# Metabolite QC pipeline - beta version 0.0.1

by: David Hughes and Laura Corbin	
date: June 3rd 2019

## This package
1. Reads in an process (un)targeted metabolite data, saving datasets in tab-delimited format for use elsewhere
2. Provides useful summary data in the form of tab-delmited text file and a PDF report.
3. Performs QC on the data using a standard pipeline and according to user-defined thresholds.

## Install MetaboQC
1. To install do the following
	
	 i. quick install
	
		a. start an R session
		b. install the MetaboQC package with

			> devtools::install_github("MRCIEU/MetaboQC")

		c. from this repo download a copy of the following files
			1. run_MetaboQC_pipeline.R
			2. parameter_file.txt
			3. QC_Report.Rmd
			
			* You can also download or clone the entire repo with
				> git clone https://github.com/MRCIEU/MetaboQC.git

	ii. alternatively you can download the package manually

		a. download a copy of the depository
		b. unzip/pack the download
		c. place the directory somewhere sensible
		d. start an R session
		e. set your working directory to the parent directory of the repo
		f. install R package with: 
			> devtools::install("MetaboQC")


## To run MetaboQC

1. Edit the paramater (parameter_file.txt) file
	1.	do not add any spaces before or after the "=" sign in the paramater file.
	2. the paramater file can be located anywhere
2. Move to the, or a, directory containing both:
	1. run_MetaboQC_pipeline.R
	2. QC_Report.Rmd
3. Make sure that R is in your environment - an often necessary step if working on an HPC.
	1. for example: module add languages/R-3.5-ATLAS-gcc-7.1.0
4. Run the MetaboQC pipeline on a terminal command line as follows:
		
		> Rscript run_MetaboQC_pipeline.R /FULL/PATH/TO/paramater_file.txt

5. We have seen that the generation of the PDF report "Project_Data_Report.pdf" fail on HPC clusters. If you experience this you can generate your PDF report on a local machine as follows.
	1. move to your newly generated MetaboQC project directory. 
		* it will take the form of "../MetaboQC_release_TODAYSDATE/"
	2. You should find an R data object called "ReportData.Rdata". Save a copy locally.
	3. Open an R session
	4. produce report with
		
			> full_path_to_QCReport_md = "FULL/PATH/TO/MetaboQC/QC_Report.Rmd"
			> full_path_to_OUTPUT_dir = "FULL/PATH/TO/MetaboQC_release_YEAR_MO_DA/"
			> full_path_to_Rdatafile = "FULL/PATH/TO/MetaboQC_release_YEAR_MO_DA/ReportData.Rdata"
			> rmarkdown::render(full_path_to_QCReport_md, output_dir = full_path_to_OUTPUT_dir, output_file = "Project_Data_Report.pdf", params = list(Rdatafile = full_path_to_Rdatafile, out_dir = full_path_to_OUTPUT_dir ) )


## QC steps in brief

### -- a detailed synopsis can be found on this git repository's wiki --

### (A) General Outline of MetaboQC
1. Read in the paramater file
2. Read in the data  -  *(typically from a commercially provided excel file)*
	* metabolite abundance
	* sample annotation
	* feature annotation
3. Write metabolite data, sample annotation and feature annotation to flat text file.
4. Estimate summary statistics on the raw data set **(step B below)**
	* write summary stats to file
5. Perfom the quality control **(step C below)**
	* using parameters passed in the parameter file 
	* write QC data set to file
6. Estimate sumary statistics on the QC'd data set **(step B below)**
	* write summary stats to file
7. Generate PDF report

### (B) Summary Statistic Estimation
1. Sample Summary Statistics
	* sample missingness
		+ all features
		+ to the exclusion of xenobiotic and\or derived variables
	* sample total peak area (TPA) **(derived variables excluded)**
		+ with all features 
		+ with complete features only (no missingness) 
	* sample outlying features count
		+ count of outlying features. 
			+ each feature analyzed within its own sample distribution
			+ outliers determined as those +/- 5 SD of the mean.
2. Feature Summary Statistics
	* feature missingness
		+ all samples
		+ to the exclusion of sample(s) with extreme missingness (> 50%)
	* distribution statistics
		+ shaprio's W-statistic of normality on raw distribution
		+ shaprio's W-statistic of normality on log10 distribution
		+ skewness
		+ kutosis
		+ N, sample size
		+ variance
		+ standard deviation
		+ coefficient of variation
		+ mean
		+ median
		+ min
		+ max
	* feature outlying sample count
		+ count of outlying samples 
			+ outliers determined as those +/- 5 SD of the mean.
3. Feature and Sample structure
	* feature:feature correlation structure **(derived variables excluded)**
		+ only includes features with at least 50 measurments
			+ or if the data set has an N<50 the missingness allowed is 0.8 * N
		+ estimate the number of independent features
		+ tag representitive features of feature clusters
	* sample:sample correaltion structure **(derived variables excluded)**
		+ principle components (PCA)
			* derived from independent features
			* missing data is imputed to the median estiamte of each feature
			* identify PC outliers
				* +/- 3,4,5 SD of mean for all significant PCs

### (C) Quality Control Steps
1. If data is from Metabolon, exclude (but retain for step 11) xenobiotic metabolites from anlaysis.
2. Estimate sample missingness and exclude extreme samples, those with a missingness >= 0.80 (or 80%) **(derived variables excluded)**
3. Estimate feature missingness and exclude extreme features, those with a missingness >= 0.80 (or 80%)
4. Re-estimate sample missingness and exclude samples >= user defined threshold (units: 0.2 or 20% missing) **(derived variables excluded)**
5. Re-estimate feature missingness and exclude features >= user defined threshold (units: 0.2 or 20% missing)
6. Estimate total peak area (the sum of all values) for each individual using complete features only and exclude samples >= user defined threshold (units: +/- SD from mean)  **(derived variables excluded)**
	* To ignore this step set to NA in parameter file
8. Build feature:feature correlation matrix on QC-data derived from steps 1-6 above **(derived variables excluded)**
	* To be included a feature must have a minimun of 50 observations, or N*0.8 observations if data set includes less than 50 individuals.
9. Identify "independent" features using data from step 8 and user defined tree cut height.
	* we retain the feature with the least missingness within a cluster, otherwise we select the first feature in the list. 	
10. Estimate principal components using indpendent features from step 9, and exclude on samples >= user defined threshold (units: +/- SD from the mean)
11. If the data is from Metabolon we place the xenobiotic metabolites back into the QC'd data set. 


**NOTE: Derived variable are those that are ratios or percentanges of two or more features already present in a data set, such as those found in Nightingale data.**

## PDF Report

1. Power Analysis
	* case control (N = 50/50) 
	* continuous
2. Metabolon data
	* feature missingness
		+ as influenced by:
			+ feature SUPER_PATHWAY (categorical function)
			+ MS method
				+ LC/MS Polar, Pos Late, Pos Early, Neg
	* sample missingness
		+ as influenced by:
			+ BOX_ID, storage box
			+ RUN_DAY, day the samples were processed on tech
		+ multivariate analysis of both BOX_ID and RUN_DAY on missingness
	* sample total peak area (TPA)
		+ as influenced by:
			+ BOX_ID, storage box
			+ RUN_DAY, day the samples were processed on tech
		+ multivariate analysis of both BOX_ID and RUN_DAY on missingness
	