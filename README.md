# Metabolite QC pipeline - beta version 0.0.1

by: David Hughes and Laura Corbin	
date: June 3rd 2019

## Install MetaboQC
1. To install do the following

	a. start an R session
	b. install MetaboQC with

		> devtools::install_github("MRCIEU/MetaboQC")

	c. alternatively you can download the package manually

		a. download the depository
		b. unzip/pack the download
		c. place the directory somewhere sensible
		d. start an R session and set working directory to your new folder
		e. insure that devtools is an available package in your environment
		f. install R package with: 
		    devtools::install("MetaboQC")

2. If installing on a cluster you may need to try the following installation

		a. Use R version 3.5
			module add languages/R-3.5-ATLAS-gcc-7.1.0
		b. Move into the downloaded directory.
			Into the parent directory of "MetaboQC" folder.
		c. Attempt a devtools installation
			devtools::install("MetaboQC")
		d. Do install any suggested updates
		e. if that fails, note the dependent packages that did not install and install them manually.
		#######
		newpacks = c("ggfortify", "ggpubr", "kableExtra", "nFactors", "pwr")
		oldpacks = installed.packages()[,"Package"]
		mispacks = newpacks[!newpacks %in% oldpacks]
		if(length(mispacks)>0){ 
	  		if (!requireNamespace("BiocManager", quietly = TRUE)){
   		 		install.packages("BiocManager")
	  		}
   	 		BiocManager::install(mispacks)
			}
		#######
		f. attempt installation of MetaboQC again:
			1) devtools::install("MetaboQC")
			2) Or write to a local directory of choice with:
				 "R CMD INSTALL -l ~/PATH/TO/yourlocal_R_library_directory/ MetaboQC"
		
		    
3. Would like to try to get this to work but it is in development

		a. devtools::install_git(url = url)

## To run the QC over your data

1. Edit the paramater (paramater_file.txt) file
	*	do not add any spaces before or after the "=" sign in the paramater file.
2. Load R into your environment, if necesasry.
3. Run the QC by running the following line of code.
	* > Rscript process_raw_data_files.R paramater_file.txt


## QC steps in brief

### detailed synopsis below

1. Read in the paramater file
2. Read in the data
	* metabolite abundance
	* sample annotation
	* feature annotation
3. Sample Summary Statistics
	* sample missingness
		+ all features
		+ exclude xenobiotics *(specific to metabolon)*
	* sample total peak area (TPA)
		+ all features
		+ complete features only (no missingness)
	* sample outlying features count
		+ count of outlying features. 
			+ each feature analyzed within its own sample distribution
			+ outliers determined as those +/- 5 SD of the mean.
4. Feature Summary Statistics
	* feature missingness
		+ all samples
		+ to the exclusion of sample with extreme missingness (> 50%)
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
5. Feature and Sample structure
	* feature:feature correlation structure
		+ estimate the number of independent features
		+ tag representitive features of feature clusters
	* sample:sample correaltion structure
		+ principle components (PCA)
			* derived from independent and complete features only
			* -- probabilistic PCA, with missing data is possible --
			* determine the number of significant PCs
			* identify PC outliers
				* +/- 3,4,5 SD of mean for all significant PCs
		+ factor analysis of significant PCs
			* identify feature strongly associated with each significant PC
			* feature annotation enrichment

## Additional analysis

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

		
# Detailed MetaboQC pipeline steps

This is a detailed log of the steps that the MetaboQC pipeline takes to process data from Nightingale, Metabolon or any other platform. 

## (1) data format

The data may be entered into the metaboQC pipeline in two forms:
	
	1) commercial supplied excel sheet
		a) Nightingale supplied excel sheet
			- sometimes accompanied by a [sample] "Metadata.xlsx" sheet
		b) Metabolon supplied excel sheet	
			- contains within...
				1) metabolite data
				2) sample metadata - in upper rows
				3) feature metadata - in first columns
	2) flat text file(s)
		a) metabolite data - features|metabolites in columns - & - samples in rows **[NOT optional]**
		b) sample metadata - sample batch variables in columns - & - samples in rows **[OPTIONAL]**
		c) feature metadata - feature batch variables in columns - & -features|metabolites in rows **[OPTIONAL]**
			++ NOTE: the MetaboQC packages holds within it a Nightingale metabolite annotation object (ng_anno). 


## (2) parameter file

To run the "run_MetaboQC_pipeline.R" pipeline script is must include a parameter file ("paramater_file.txt") argument must be passed to it. 

Running the script takes the form:

> Rscript run_MetaboQC_pipeline.R myfullpath/parameter_file.txt

The parameter file includes:

	1) a project name
	2) full path to the directory holding your data.
		- This directory must inlcude all files subsequently shared in the parameter file.
	3) the name of the commercially supplied excel sheet or the flat text file holding the metabolite data
		- flat text file should have samples in rows, features|metabolites in columns
	4) the name of flat text feature annotation file 
		- NA, if no such file is available
		- otherwise, the first column of data should match the feature (column) names in (4)
			+ NOTE: if you are process Metabolon data using a flat text file (NOT the commercial excel sheet)
				(i) then it would be advisable to have a feature annotation file that has a column called "SUPER_PATHWAY" that identifies which metabolites|features are "Xenobiotics".
					+ "Xenobiotics" are treated uniquely in the MetaboQC pipeline
	5) the name of the flat text sample annotation | batch file
		- NA, if no such file is available
		- otherwise, the first column of data should match the sample (row) names in (4)
	6) declaration of the platform used
		- Nightingale, Metabolon, or other
	7) the allowed level of feature missingness
	8) the allowed level of sample missingness
	9) the sample total peak area filtering level, in standard deviations deviations of the mean
	10) the sample principal component (PC1-PC2) filtering level, in standard deviations of the mean
	
## (3) run pipeline

	> Rscript run_MetaboQC_pipeline.R parameter_file.txt

## (4) pipeline steps - in detail

### (I) intiate

	1) check for parameter file
	2) record date
	3) process project name and data directory, as provided in parameter file
	4) make a new directory in provided data directory to place MetaboQC output
	5) start a log file - placed in the newly made MetaboQC data directory.
	6) process remaining arguments in the parameter file
	
### (II) read in the data

	1) evaluate if METABOLITE data file is a flat text file or excel file
		(a) if provided file is a flat text file
			(i) read in file
				- check if rownames are numeric. if yes, assign column 1 as row names.
			(ii) if platform was declared as Nightingale attempt to edit feature names to match data in MetaboQC's ng_anno (Nighthingale annotation) object.
	2) evaluate if a flat text FEATURE annotation|batch file name was provided
		(a) if a file name was provided read it in
			(i) read in file
				- check if rownames are numeric. if yes, assign column 1 as row names.
					- column 1 IDs should match the feature (columns) names in metabolite data file 
			(ii) if platform was declared as Nightingale attempt to edit feature names (rows) to match data in MetaboQC's ng_anno (Nighthingale annotation) object.
	3) evaluate if a flat text SAMPLE annotation|batch file name was provided
		(a) if a file name was provided read it in
			(i) read in file
				- check if rownames are numeric. if yes, assign column 1 as row names.
					- column 1 IDs should match the sample (rows) names in metabolite data file
	4) generate a working data set object - defined as a list.
		(a) if platform was declared as Nightingale 
			(i) the feature annotation data in the object ng_anno will be added to the feature annotation data sheet. 
	5) evaluate if METABOLITE data file is an excel sheet
		(a) if yes and platform is delcared as Nightingale
			(i) read in data with the function read.in.nightingale()
		(b) if yes and platform is declared as Metabolon
			(i) read in data with the function read.in.metabolon()
			
### (III) Estimate summary statistics

	