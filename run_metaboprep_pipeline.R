###########################################
## metaboprep pipeline
## 
##  by: Laura Corbin & David Hughes 
##	date: May 14th 2019
###########################################

######################################
## RUN DIRECTIONS:
## - run the script by running the line
## > Rscript run_metaboprep_pipeline.R paramater_file.txt 
######################################

#######################################
##
## I) Load the metaboprep R package
##
#######################################
library(metaboprep)

## define nightingale object
# ng_anno = metaboprep:::ng_anno

#########################################
##
## II) Read in arguments from command line
##
#######################################
## You should pass a single argument
## that is the path to the paramater_file.txt
args = commandArgs(trailingOnly=TRUE)

## Check that you passed an argument to the script
if(length(args) != 1){
  stop( 
    paste0("You must provide a single argument, the paramater file, when you call the script.\n\tEXAMPLE:$ Rscript --vanilla run_metaboprep_pipeline.R parameter_file.txt.\n"),
    call.=FALSE)
} 

## record todays date
today = Sys.Date()
today = gsub("-","_",today)

#######################################
##
## III) Parse the info in the paramater file
##
#######################################
## Read in the paramater file
pfile = read.table(args[1], header = FALSE, sep = "=", as.is = TRUE)

##################################
## (A) What is the project name?
##################################
project <- as.character( pfile[1,2] )

##################################
## (B) What is the full path to the directory containing the DATA?	
##################################
data_dir <- as.character( pfile[2,2] )

## make sure data directory ends with a "/"
if(substring(data_dir, nchar(data_dir)) != "/"){
  data_dir = paste0(data_dir,"/")
}

#######################################
##
## IV) Make a new sub directory
##
#######################################
## check for spaces in fle paths
dd = data_dir
dd = gsub(" ","\\\\ ", dd)
cmd = paste0("mkdir -p ", dd, "metaboprep_release_", today)
system(cmd)

#######################################
##
## V) Start writing a log file
##
#######################################
logfilename = paste0(data_dir, "metaboprep_release_", today, "/", project, "_", today,  "_logfile.txt")
sink(file = logfilename , split = TRUE  )

cat(paste0("I. Setting up your pipeline\n"))
cat(paste0("\t-Your data directory is: ", data_dir, "\n"))


#######################################
##
## III - CONT'd) Parse the info in the paramater file
##
#######################################

##################################
## (C) Name of metabolite data file
##################################
METABO_file2process = as.character( pfile[3,2] )
## identify the file type
xl_ftype = c(".xls",".xlsx", ".XLS", ".XLSX")
isexcel = sum(unlist( sapply(xl_ftype, function(x){ grep(x, METABO_file2process) } ) ) )

flat_ftype = c(".csv",".txt", ".tsv", ".TXT", ".CSV", ".TSV")
isflat = sum(unlist( sapply(flat_ftype, function(x){ grep(x, METABO_file2process) } ) ) )

cat(paste0("\t- Your metabolite data file is: ", METABO_file2process, "\n"))
if(isexcel > 0){
  cat(paste0("\t\tYour metabolite data file was identified as an excel sheet\n\t\tand will be processed as a commercial source file.\n"))  
  } else {
    if(isflat > 0){
      cat(paste0("\t\tYour metabolite data file was identified as a previously processed flat text file.\n"))    
    } else {
      stop( paste0("\t\tUnable to identify the type of file you provided.\n\t\tPlease be sure it is an xls, xlsx, txt, or csv.\n"), call.=FALSE )    
    } 
  }
##################################
## (D) Name of feature annotation file
##     If you are reading in a commercial source EXCEL file 
##     can be NA. If reading in a pre-processed flat text file
##     and data is from Metabolon, it would be best to provide a 
##     column called "SUPER_PATHWAY" to identify "Xenobiotics"
##################################
FeatureAnno_file2process = as.character( pfile[4,2] )
if( !is.na(FeatureAnno_file2process)){
  cat(paste0("\t- Your provided feature annotation file to process is: ", FeatureAnno_file2process, "\n"))  
} else{
  cat(paste0("\t- You have NOT provided a feature annotation file to process.\n"))  
}

##################################
## (E) Name of sample (batch) annotation file
##     If you are reading in a commercial source EXCEL file 
##     can be NA.
##################################
SampleAnno_file2process = as.character( pfile[5,2] )
if( !is.na(SampleAnno_file2process)){
  cat(paste0("\t- Your provided sample | batch annotation file to process is: ", SampleAnno_file2process, "\n"))  
} else{
  cat(paste0("\t- You have NOT provided a sample | batch annotation file to process.\n"))  
}

##################################
## (F) What platform does your data come from?
##################################
platform = as.character( pfile[6,2] )
cat(paste0("\t- Your declared platform is: ", platform, "\n"))

##################################
## (G) QC values
##################################
feature_missingness = as.numeric( pfile[7,2] )
cat(paste0("\t- Feature filtering: Your declared feature missingness level is: ", feature_missingness, "\n"))

sample_missingness = as.numeric( pfile[8,2] )
cat(paste0("\t- Sample filtering: Your declared sample missingness level is: ", sample_missingness, "\n"))

total_peak_area_SD = as.numeric( pfile[9,2] )
cat(paste0("\t- Sample filtering: Your declared total peak area filter level, in standard deviations from the mean is: ", total_peak_area_SD, "\n"))

outlier_udist = as.numeric( pfile[10,2] )
cat(paste0("\t- Sample outliers at features: Your declared that the interquartile range unit distance from the median of each feature to call a sample an outlier to be: ", outlier_udist, "\n"))

outlier_treatment = as.character( pfile[11,2] )
cat(paste0("\t- Sample outliers at features: Your declared that outliers, for the purposes of the PCA & PCA only, should be: ", outlier_treatment, "\n"))

tree_cut_height = as.numeric( pfile[12,2] )
cat(paste0("\t- Metabolite independence: Your declared tree cut height is: ", tree_cut_height, "\n\n"))

PC_outlier_SD = as.numeric( pfile[13,2] )
cat(paste0("\t- Sample filtering: Your declared principal component (PC1 and PC2) exclusion, in standard deviations from the mean is: ", PC_outlier_SD, "\n"))

## Nightingale derived variable exclusions
## when evaluting SAMPLE quality for QC
derived_var_exclusion = pfile[14,2] 
if(platform == "Nightingale"){
  if(derived_var_exclusion == TRUE){
  cat(paste0("\t- You have declared that Nightingale derived variables should be excluded from data filtering steps.\n\n"))
  }
}

## Mass Spec Run Mode for each metabolite.
## This variable defines the column name, in the feature_annotation_file, indexing the run mode string(s).
## There should in turn be column name(s) in the sample_annotation_file that match the run mode string(s)
##    and hold the batch IDs for each sample, in that run mode. 
feat_anno_run_mode_col = as.character( pfile[15,2] )
if( !is.na(feat_anno_run_mode_col)){
  cat(paste0("\t- You have declared that the column name in the feature annotation file holding the run mode variables is: ", feat_anno_run_mode_col, "\n"))  
} 

## Should a scatter plot, histogram, and table of summary statsitics be
## written to PDF for visual inspection? TRUE or FALSE?
plot_feature_distributions = pfile[16,2] 
if(plot_feature_distributions==TRUE){
  cat(paste0("\t- You have declared that a scatter plot, histogram, and table of summary statsitic for each feature in the data set should be written to pdf.\n"))    
}


#######################################
##
## VI) Reading in the data
##
#######################################

##################################
## (A) process Metabolite data file
##################################
n = paste0(data_dir, METABO_file2process)
##
if(isflat > 0){
  if( length(grep(".csv", n )) > 0 ){
    cat(paste0("\t- Reading in your csv metabolite file\n"))
    metabolitedata = read.csv(n, header = TRUE, as.is = TRUE, na.strings = c("NA","NDEF", "TAG", -1, -9), row.names = 1 )
  }
  ####
  if( length( c( grep(".txt", n ), grep(".tsv", n )  ) )  > 0 ){
    cat(paste0("\t- Reading in your txt|tsv metabolite file\n"))
    metabolitedata = read.table(n, header = TRUE, as.is = TRUE, sep = "\t", na.strings = c("NA","NDEF", "TAG", -1, -9), row.names = 1 )
  }
  ## remove any possible commas
  metabolitedata = apply(metabolitedata, 2, function(x){
    o = sapply(x, function(y){
      gsub(",","",y)
      })
    return(o)
    })
  metabolitedata = as.data.frame(metabolitedata)
  
  ## format metabolite data:
  ## look to see of row names are just numerics. if yes redefine rownames and column 1 values
  editrownames = sum( rownames(metabolitedata) == 1:nrow(metabolitedata) ) /nrow(metabolitedata)
  if(editrownames == 1){
    cat(paste0("\t\t- Assuming sample IDs are in column 1 and redefining rownames\n"))
    rownames(metabolitedata) = as.character(metabolitedata[,1])
    metabolitedata = metabolitedata[,-1]
  }
  ## look to see of column names are just numerics. R will add X to numberic column names
  editcolnames = sum( substring(colnames(metabolitedata) ,1,1) == "X", na.rm = TRUE ) / ncol(metabolitedata)
  if(editcolnames == 1){
    cat(paste0("\t\t- Column names are numeric. Adding 'featID_' prefix to each.\n") )
    numeric_ids = substring(colnames(metabolitedata) , 2, nchar(colnames(metabolitedata)) ) 
    new_col_id = paste0("featID_", as.character(numeric_ids))
    colnames(metabolitedata) = new_col_id
  }
  ## if platform is Nightingale edit metabolite names
  if( platform == "Nightingale"){
    cat(paste0("\t\t- Your defined platform is Nightingale,\n\t\t so editing metabolite names in an attempt to match the metaboprep annotation file.\n"))
    ## edit column metabolite names
    colnames(metabolitedata) = gsub("_.", "pct", colnames(metabolitedata))
    colnames(metabolitedata) = gsub("%", "pct", colnames(metabolitedata))
    colnames(metabolitedata) = gsub("/", "_", colnames(metabolitedata))
    colnames(metabolitedata) = gsub("\\.", "", colnames(metabolitedata))
    colnames(metabolitedata) = gsub("-", "", colnames(metabolitedata))
    colnames(metabolitedata) = gsub("_", "", colnames(metabolitedata))
    colnames(metabolitedata) = tolower(colnames(metabolitedata))
  }
  ### insure everything is numeric
  ids = rownames(metabolitedata)
  metabolitedata = apply(metabolitedata, 2, function(x){ as.numeric(as.character(x)) })
  rownames(metabolitedata) = ids
  ### END OF "isflat" if statement
}

##################################
## (B) Checking for and process a 
##     flat text Feature Annotation File
##################################
if( !is.na(FeatureAnno_file2process) ){
  ## full path to feature annotation file
  n = paste0(data_dir, FeatureAnno_file2process)
  ##
  if( length(grep(".csv", n )) > 0 ){
    cat(paste0("\t- Reading in you csv feature annotation file\n"))
    featuredata = read.csv(n, header = TRUE, as.is = TRUE, quote = "", fill = TRUE)
    # featuredata = readr::read_delim(n, show_col_types = FALSE)
  }
  ####
  if( length( c( grep(".txt", n ), grep(".tsv", n )  ) ) > 0 ){
    cat(paste0("\t- Reading in you txt feature annotation file\n"))
    #featuredata = read.table(n, header = TRUE, as.is = TRUE, sep = "\t", quote = "", fill = TRUE)
    featuredata = readr::read_delim(n, show_col_types = FALSE)
    featuredata = as.data.frame(featuredata)
  } 
  ## format featuredata data
  editrownames = sum( rownames(featuredata) == 1:nrow(featuredata) ) /nrow(featuredata)
  if(editrownames == 1){
    ## redefine only if the number of unique strings is the same as the number of rows
    if( length(unique(featuredata[,1])) == nrow(featuredata) ){
      cat(paste0("\t\t- Assuming feature IDs are in column 1 and redefining rownames\n"))
      ## looking to see if column names of metabolite data file were also numeric. If yes then "X" was added by R.
      ##  this was removed and "featID_" was added as a prefix. So we have to do the same here for name matching
      ##  during filtering steps later.
      if(editcolnames == 1){
      cat(paste0("\t\t- Column were numeric so will also add 'featID_' as a prefix to each row names here.\n") )
        rownames(featuredata) = paste0( "featID_", as.character( featuredata[,1] ) )
      } else {
        rownames(featuredata) = as.character(featuredata[,1])
        #featuredata = featuredata[,-1]  
      }
      
    }
  }
  
  ## Make sure there is a "feature_names" and that it has same values as rownames
  featuredata$feature_names = rownames(featuredata)

  ##
  if( platform == "Nightingale"){
    cat(paste0("\t\t- Your defined platform is Nightingale,\n\t\t  so editing metabolite names in an attempt to match the metaboprep annotation file.\n"))
    ## edit column metabolite names
    rownames(featuredata) = gsub("_.", "pct", rownames(metabolitedata))
    rownames(featuredata) = gsub("%", "pct", rownames(featuredata))
    rownames(featuredata) = gsub("/", "_", rownames(featuredata))
    rownames(featuredata) = gsub("\\.", "", rownames(featuredata))
    rownames(featuredata) = gsub("-", "", rownames(featuredata))
    rownames(featuredata) = gsub("_", "", rownames(featuredata))
    rownames(featuredata) = tolower(rownames(featuredata))
  }
}

##################################
## (C) Checking for and process a 
##     flat text Sample Annotation File
##################################
if( !is.na(SampleAnno_file2process) ){
  ## full path to feature annotation file
  n = paste0(data_dir, SampleAnno_file2process)
  ##
  if( length(grep(".csv", n )) > 0 ){
    cat(paste0("\t- Reading in you csv sample annotation file\n"))
    sampledata = read.csv(n, header = TRUE, quote = "", as.is = TRUE)
  }
  ####
  if(length( c( grep(".txt", n ), grep(".tsv", n )  ) ) > 0 ){
    cat(paste0("\t- Reading in you txt sample annotation file\n"))
    sampledata = read.table(n, header = TRUE, quote = "", as.is = TRUE, sep = "\t")
  } 
  ## format sampledata data
  editrownames = sum( rownames(sampledata) == 1:nrow(sampledata) ) /nrow(sampledata)
  if(editrownames == 1){
    cat(paste0("\t\t- Assuming sample IDs are in column 1 and redefining rownames\n"))
    rownames(sampledata) = as.character(sampledata[,1])
    #sampledata = sampledata[,-1]
  }
}

##################################
## (D) Generate a WORKING data set 
##     for flat text source files
##################################
if(isflat > 0){
  if( !exists( x = "sampledata" ) ){
    sampledata = data.frame(SampleID = rownames(metabolitedata))
  }
  ####
  if( !exists( x = "featuredata" ) ){
    featuredata = data.frame(feature_names = colnames(metabolitedata))
    rownames(featuredata) = as.character( featuredata[,1] )
  }
  ## add Nightingale feature annotation data
  if(platform == "Nightingale"){
    m = match( rownames(featuredata), ng_anno$metabolite)
    featuredata = cbind(featuredata, ng_anno[m, -1])
  }
  ###
  mydata = list(metabolitedata = metabolitedata, sampledata = sampledata, featuredata = featuredata)
  }



################################## 
## (E) IF Data file is Excel
##################################
if(isexcel > 0){

  #############################
  ### Process if Nightingale
  #############################
  if(platform == "Nightingale"){
    cat( paste0("II. Processing your Nightingale data.\n") )
    if( !is.na(pfile[3,2]) ){
        ## Read in the raw data, excel files, write to flat text in the data directory and return all data as a list
        mydata = read.in.nightingale( file2process = METABO_file2process, data_dir = data_dir, projectname = project )
        cat( paste0("\t- Your raw Nightingale data has been read in and converted to working tab delimited text files.\n\n") )

      } 
  }


  #############################
  ### Process if Metabolon
  #############################
  if(platform == "Metabolon"){
    cat( paste0("II. Processing your Metabolon data.\n") )
    if( !is.na(pfile[3,2]) ){
      ## Read in the raw data, excel files, write to flat text in the data directory and return all data as a list
      mydata = read.in.metabolon( file2process = METABO_file2process, data_dir = data_dir, projectname = project )
      cat( paste0("\t- Your raw Metabolon data has been read in and converted to working tab delimited text files.\n\n") )

      } 
  }

}



## READING IN DATA DONE
cat( paste0("III. Your data has been read in.\n\n") )
cat( paste0("\t-Your data has ", nrow(mydata$metabolitedata), " individuals and ", ncol(mydata$metabolitedata), " metabolites.\n\n") )
cat( paste0("\t-There are also ", ncol(mydata$sampledata), " sample annotation|batch variables.\n\n") )
cat( paste0("\t-There are also ", ncol(mydata$featuredata), " feature annotation|batch variables.\n\n") )
if(length(mydata)>3){
  for(i in 4:length(mydata)){
    cat( paste0("\t-Your data also has an additional metabolite data tab named ", names(mydata)[i] ," with ", nrow(mydata[[i]]), " individuals and ", ncol(mydata[[i]]), " metabolites.\n\n") )
  }  
}

#########################
##
## (VII)  Normalize Metabolon or Other (MS) Data
##
#########################
if(platform == "Metabolon"){
  cat( paste0("Normalization. Performing normalization on Metabolon Data.\n\n") )
  if(!is.na(feat_anno_run_mode_col)){
      cat( paste0("\t- Performing normalization with parameter file provided feature annotation column '",feat_anno_run_mode_col,"'.\n") )

    norm_metabolite_data = batch_normalization( wdata = mydata$metabolitedata, 
        feature_data_sheet =  mydata$featuredata, 
        sample_data_sheet = mydata$sampledata, 
        feature_runmode_col = feat_anno_run_mode_col, 
        batch_ids = NULL  )

    ## save the raw data as another object in the list
    mydata$raw_metabolitedata = mydata$metabolitedata

    ## redefine the working metabolitedata object
    mydata$metabolitedata = norm_metabolite_data

    ## remove the unnecessary data frame
    rm(norm_metabolite_data)
    cat( paste0("\t- Normalization completed.\n\n") )
    } else {
      ##################################################
      ## look for run mode information in feature data
      ##################################################
      cat( paste0("\t- Looking for run mode information automatically given Metabolon standard data release formatting.\n") )
      fanno_col_number = which(colnames(mydata$featuredata) %in% c("PLATFORM","platform"))
      if(length(fanno_col_number) == 1){
        runmode = unlist( mydata$featuredata[,fanno_col_number] )
        ## remove "LC/MS " if present
        runmode = gsub("LC\\/MS\\ ","",runmode)
        ## remove spaces 
        runmode = gsub(" ","",runmode)
        ## make lower case
        runmode = tolower(runmode)

        ## redefine runmode string in feature data file
        mydata$featuredata[,fanno_col_number] = runmode

        batchrunmodes = unique(runmode)
      } else {
        cat(paste0("\t- NOTE: Unable to identify a column header called 
            'PLATFORM' or 'platform' in the feature data sheet.
             This is necessary to perform batch normalization\n\n") )

      }
      ##################################################
      ## look for batch info in the sample sheet
      ##################################################
      n = tolower( colnames(mydata$sampledata) )
      ## remove spaces 
      n = gsub(" ","",n)
      ## remove underscore 
      n = gsub("_","",n)
      ## remove dots 
      n = gsub("\\.","",n)

      ## redefine column names
      k = which(n %in% runmode)
      colnames(mydata$sampledata)[k] = n[k]

      if(length(k) == length(batchrunmodes) ){
        ## perfom normalization
        cat( paste0("\t- Performing normalization with automatically identified Metabolon standard data release formatting column '",colnames(mydata$featuredata)[fanno_col_number],"'.\n") )

        norm_metabolite_data = batch_normalization( wdata = mydata$metabolitedata, 
            feature_data_sheet =  mydata$featuredata, 
            sample_data_sheet = mydata$sampledata, 
            feature_runmode_col = fanno_col_number, 
            batch_ids = NULL  )

        ## save the raw data as another object in the list
        mydata$raw_metabolitedata = mydata$metabolitedata

        ## redefine the working metabolitedata object
        mydata$metabolitedata = norm_metabolite_data

        ## remove the unnecessary data frame
        rm(norm_metabolite_data)
        cat( paste0("\t- Normalization completed.\n\n") )

      } else {
        cat(paste0("\t- NOTE: Unable to identify a column headers in sample sheet
            that match the platform run modes found in the feature data sheet.
            This should be something like neg, polar, pos early, pos late.\n") )

        cat( paste0("\t- NOTE: We will take the ScaledImpData data and remove
            the imputed data to extract the normalized data.\n\n") )
        
        scaled_imputed_data_tab = grep("ScaledImp", names(mydata))
        if(length(scaled_imputed_data_tab) == 1){
          ndata = mydata[[scaled_imputed_data_tab]]
          
          if(sum(is.na(mydata$metabolitedata))>0){
            ndata[is.na(mydata$metabolitedata)] = NA  
          }
          ## save the raw data as another object in the list
          mydata[["raw_metabolitedata"]] = mydata$metabolitedata

          ## redefine the working metabolitedata object
          mydata$metabolitedata = ndata 
          
          ## remove the unnecessary data frame
          rm(ndata) 
          cat( paste0("\t- Normalization completed.\n\n") )

        } else {
          cat( paste0("\t- NOTE: Unable to identify a 'ScaledImp' data tab in the excel file.
            No normalization carried out.\n\n") )
        }    
    }
  }
}


#########################
##
## (VII)  Normalize Other MS Data
##
#########################
if( !is.na(feat_anno_run_mode_col) & platform == "Other" ){
  cat( paste0("Normalization. Performing normalization parameter file provided feature annotation column '",feat_anno_run_mode_col,"'.\n") )

  norm_metabolite_data = batch_normalization( wdata = mydata$metabolitedata, 
      feature_data_sheet =  mydata$featuredata, 
      sample_data_sheet = mydata$sampledata, 
      feature_runmode_col = feat_anno_run_mode_col, 
      batch_ids = NULL  )

  ## save the raw data as another object in the list
  mydata$raw_metabolitedata = mydata$metabolitedata

  ## redefine the working metabolitedata object
  mydata$metabolitedata = norm_metabolite_data

  ## remove the unnecessary data frame
  rm(norm_metabolite_data)

  cat( paste0("        - Normalization completed.\n\n") )

}

#########################
##
## (VII)	Estimate  Summary Statistics
##
#########################
cat( paste0("IV. Estimating Summary Statistics On Raw Data Set.\n") )

##################################
## A. Summary Statistics for samples
##################################
cat( paste0("\ta. Estimating summary statistics for samples\n") )

## Is this Metabolon data??
##  -- is the column SUPER_PATHWAY present in the feature data
##  -- if yes, exclude Xenobiotics from one of the missingness estimate
if( length(mydata$featuredata$SUPER_PATHWAY) > 0){
  w = which( mydata$featuredata$SUPER_PATHWAY %in% c("xenobiotics", "Xenobiotics") )
  xeno_names = mydata$featuredata$feature_names[w]
  samplesumstats = sample.sum.stats( wdata = mydata$metabolitedata, feature_names_2_exclude = xeno_names, outlier_udist = outlier_udist )
} else {
  ## Is this Nightingale data??
  ##  -- is the column derived_features present in the feature data
  ##  -- if yes, exclude derived variables from one of the missingness estimate
  if( length(mydata$featuredata$derived_features) > 0 & derived_var_exclusion == "TRUE" ){
    w = which( mydata$featuredata$derived_features == "yes") 
    derivedfeature_names = as.character( mydata$featuredata$feature_names[w] )
    samplesumstats = sample.sum.stats( wdata = mydata$metabolitedata, feature_names_2_exclude = derivedfeature_names, outlier_udist = outlier_udist )
  } else {
      samplesumstats = sample.sum.stats( wdata = mydata$metabolitedata, outlier_udist = outlier_udist)
    }
  }

### write sample sum stats to file
cat( paste0("\t\t- Writing sample summary statistics to file.\n") )

## make a sum stats directory in data_dir
## evaluate and account for spaces in file paths
dd = data_dir
dd = gsub(" ","\\\\ ", dd)
###
cmd = paste0("mkdir -p ", dd,  "metaboprep_release_", today, "/", "sumstats")
system(cmd)
## make a raw_dataset directory inside the sumstats folder
cmd = paste0("mkdir -p ", dd,  "metaboprep_release_", today, "/", "sumstats/raw_dataset")
system(cmd)


### SAMPLES
if( "sampledata" %in% names(mydata) ){
  # mydata$sampledata = cbind(mydata$sampledata, samplesumstats)
  samplesumstats = cbind(mydata$sampledata, samplesumstats)
}
n = paste0(data_dir,  "metaboprep_release_", today, "/sumstats/raw_dataset/", project, "_", today, "_sample_anno_sumstats.txt")
# write.table(mydata$sampledata, file = n,
write.table( samplesumstats, file = n,
           row.names = FALSE, col.names = TRUE, 
           sep = "\t", quote = FALSE)

##################################
## B. Summary Statistics for features
##################################
cat( paste0("\tb. Estimating summary statistics for features.\n") )

### sample missingness
if( length(samplesumstats$sample_missingness_w_exclusions) > 0 ){
  sammis = samplesumstats$sample_missingness_w_exclusions
  } else {
    sammis = samplesumstats$sample_missingness
  }

### feature names to exclude
if( length(mydata$featuredata$derived_features) > 0 & derived_var_exclusion == "TRUE" ){
  w = which( mydata$featuredata$derived_features == "yes") 
  fn2e = as.character( mydata$featuredata$feature_names[w] )
} else {
    fn2e = NA
  }


### RUN feature summary stats funtion
featuresumstats = feature.sum.stats( wdata = mydata$metabolitedata,
                                      sammis = sammis, 
                                      tree_cut_height = tree_cut_height,
                                      outlier_udist = outlier_udist,
                                      feature_names_2_exclude = fn2e )


### write feature sum stats to file
cat( paste0("\t\t- Writing feature summary statistics to file.\n") )

if( "featuredata" %in% names(mydata) ){
  featuresumstats$table = cbind( mydata$featuredata, featuresumstats$table[,-1])
}
n = paste0(data_dir, "metaboprep_release_", today, "/sumstats/raw_dataset/", project, "_", today, "_feature_anno_sumstats.txt")
write.table(featuresumstats$table, file = n,
            row.names = FALSE, col.names = TRUE, 
            sep = "\t", quote = TRUE)
 
##################################
## C. PC outliers
##################################
cat( paste0("\tc. Performing principle component analysis and identifying outliers.\n") )

## identify independent feature names as reported in featuresumstats
w = which(featuresumstats$table$independent_features_binary == 1)
indf = as.character( featuresumstats$table[w,1] )
if( sum( indf %in% colnames(mydata$metabolitedata) ) == 0 ){
  indf = as.character( featuresumstats$table$feature_names[w] )  
}
PCs_outliers = pc.and.outliers(metabolitedata =  mydata$metabolitedata, 
                               indfeature_names = indf)

### write sample sum stats to file
cat( paste0("\t\t- Re-Writing sample summary statistics to file to include PCs.\n") )

### SAMPLES
samplesumstats = cbind( samplesumstats, PCs_outliers[[1]])
n = paste0(data_dir,  "metaboprep_release_", today, "/sumstats/raw_dataset/", project, "_", today, "_sample_anno_sumstats.txt")  
write.table( samplesumstats, file = n,
            row.names = FALSE, col.names = TRUE, 
            sep = "\t", quote = FALSE)


### write the variance explained by pcs out to file
cat( paste0("\t\t- Writing PC statistics to file.\n\n") )

varexp = data.frame(VarExp = PCs_outliers[[2]])
n = paste0(data_dir, "metaboprep_release_", today, "/sumstats/raw_dataset/", project, "_", today, "_pc_varexp.txt")
write.table(varexp, file = n,
            row.names = TRUE, col.names = TRUE, 
            sep = "\t", quote = FALSE)

n = paste0(data_dir, "metaboprep_release_", today, "/sumstats/raw_dataset/", project, "_", today, "_featuretree.Rdata")
feature_tree = featuresumstats[[2]]
save(feature_tree, file = n)


#############################
##
## Make a raw data set object
##
#############################
raw_data = list(metabolite_data = mydata$metabolitedata,
  sample_data = samplesumstats,
  feature_data = featuresumstats$table,
  feature_tree = feature_tree,
  varexp = varexp
  )


#########################
##
## (VIII) Perform QC
##
#########################
##################################
## A. Perform QC
##################################
cat( paste0("V. Performing data filtering.\n") )

dd = data_dir
dd = gsub(" ","\\\\ ", dd)
##
cmd = paste0("mkdir -p ", dd, "metaboprep_release_", today, "/", "filtered_data")
system(cmd)

### xenobiotics to exclude
w = which( mydata$featuredata$SUPER_PATHWAY  %in% c("xenobiotics", "Xenobiotics") ) 
xeno_names = mydata$featuredata$feature_names[w]
if( length(xeno_names) == 0){xeno_names = NA}

### derived variables to exclude
w = which( mydata$featuredata$derived_features == "yes" ) 
derived_names = as.character( mydata$featuredata$feature_names[w] )
if(length(derived_names) == 0){derived_names = NA}
if(derived_var_exclusion != "TRUE"){derived_names = NA}

### execute super function
cat( paste0("\ta. Performing data filtering.\n") )

dataQC = perform.metabolite.qc(wdata = mydata$metabolitedata,
                               fmis = feature_missingness, 
                               smis = sample_missingness, 
                               tpa_out_SD = total_peak_area_SD,
                               outlier_treatment = outlier_treatment, ## options are "leave_be", "turn_NA", "winsorize"
                               winsorize_quantile = 1,                ## winsorize to what quantile of remaining (not outlier) values
                               outlier_udist = outlier_udist,
                               PC_out_SD = PC_outlier_SD,
                               tree_cut_height = tree_cut_height,
                               feature_colnames_2_exclude = xeno_names,
                               derived_colnames_2_exclude = derived_names
)

#################################
## B. write QC data to file
#################################
cat( paste0("\tb. Writing QC data to file.\n\n") )


#############################
##
## B.1. Make a QCing data set object
##
#############################
qcing_data = list(metabolite_data = dataQC$wdata,
  exclusion_data = dataQC$exclusion_data,
  feature_sumstats = dataQC$featuresumstats$table,
  feature_tree = dataQC$featuresumstats$tree,
  pcs = dataQC$pca$pcs,
  varexp = dataQC$pca$varexp,
  accelerationfactor = dataQC$pca$accelerationfactor,
  nparallel = dataQC$pca$nsig_parrallel
  )


##################################
## B.2. Add sample and feature data to qcdata
##################################
temp = dataQC$wdata
m = match( rownames(temp) , mydata$sampledata[,1] )
n = match( colnames(temp) , mydata$featuredata[,1] )
if( length(n) == sum(is.na(n)) ){
  n = match( colnames(temp) , mydata$featuredata$feature_names )
}
qcdata = list(metabolitedata = temp, 
  sampledata = as.data.frame( mydata$sampledata[m,] ), 
  featuredata = as.data.frame( mydata$featuredata[n,] ) )
rm(temp)

if( colnames(qcdata$sampledata)[1] == "mydata$sampledata[m, ]" ){ colnames(qcdata$sampledata)[1] = "SampleID" }
if( colnames(qcdata$featuredata)[1] == "mydata$featuredata[n, ]" ){ colnames(qcdata$featuredata)[1] = "feature_names" }

##################################
## B.3. Write to file
##################################
## qc metabolite data
n = paste0(data_dir, "metaboprep_release_", today, "/filtered_data/", project, "_", today, "_Filtered_metabolite_data.txt")
write.table(qcdata$metabolitedata, file = n,
            row.names = TRUE, col.names = TRUE, 
            sep = "\t", quote = FALSE)

## qc sample data
n = paste0(data_dir, "metaboprep_release_", today, "/filtered_data/", project, "_", today, "_Filtered_sample_data.txt")
write.table(qcdata$sampledata, file = n,
            row.names = FALSE, col.names = TRUE, 
            sep = "\t", quote = FALSE)

## qc metabolite data
n = paste0(data_dir, "metaboprep_release_", today, "/filtered_data/", project, "_", today, "_Filtered_feature_data.txt")
write.table(qcdata$featuredata, file = n,
            row.names = FALSE, col.names = TRUE, 
            sep = "\t", quote = FALSE)



#####################################
##
## (IX)  Estimate  Summary Statistics 
##        on the QC'd Data Set
##
#####################################
cat( paste0("VI. Estimating Summary Statistics on Filtered Data Set.\n") )

##################################
## A. Summary Statistics for samples
##################################
cat( paste0("\ta. Estimating summary statistics for Filtered samples\n") )

## A.1. Estiamte sum stats
## Is this metabolon data?? 
##  -- is the column SUPER_PATHWAY present in the feature data
##  -- if yes, exclude Xenobiotics from one of the missingness estimate
if( length(qcdata$featuredata$SUPER_PATHWAY) > 0){
  w = which( qcdata$featuredata$SUPER_PATHWAY  %in% c("xenobiotics", "Xenobiotics") ) 
  xeno_names = rownames(qcdata$featuredata)[w]
  samplesumstats = sample.sum.stats( wdata = qcdata$metabolitedata, feature_names_2_exclude = xeno_names, outlier_udist = outlier_udist )
} else {
  ## Is this Nightingale data?? 
  ##  -- is the column derived_features present in the feature data
  ##  -- if yes, exclude derived variables from one of the missingness estimate
  if( length(qcdata$featuredata$derived_features) > 0 & derived_var_exclusion == "TRUE" ){
    w = which( qcdata$featuredata$derived_features == "yes") 
    derivedfeature_names = as.character( qcdata$featuredata$feature_names[w] )
    samplesumstats = sample.sum.stats( wdata = qcdata$metabolitedata, feature_names_2_exclude = derivedfeature_names, outlier_udist = outlier_udist )
  } else {
      samplesumstats = sample.sum.stats( wdata = qcdata$metabolitedata, outlier_udist = outlier_udist)
    }
}

### A.2. Write sample sum stats to file
cat( paste0("\tb. Writing filtered sample summary statistics to file.\n") )

## make a filtered_dataset directory inside the sumstats directory in data_dir
## evaluate and account for spaces in file paths
dd = data_dir
dd = gsub(" ","\\\\ ", dd)
## system command
cmd = paste0("mkdir -p ", dd,  "metaboprep_release_", today, "/", "sumstats/filtered_dataset")
system(cmd)


### WRITE
if( "sampledata" %in% names(qcdata) ){
  ## add sample stats to the sample annotation file
  samplesumstats = cbind(qcdata$sampledata, samplesumstats[,-1])
}  
n = paste0(data_dir,  "metaboprep_release_", today, "/sumstats/filtered_dataset/", project, "_", today, "_sample_anno_sumstats.txt") 
write.table(samplesumstats, file = n,
            row.names = FALSE, col.names = TRUE, 
            sep = "\t", quote = FALSE)


##################################
## B. Summary Statistics for features
##################################
cat( paste0("\tc. Estimating summary statistics for filtered features.\n") )

### sample missingness
if( length(samplesumstats$sample_missingness_w_exclusions) > 0 ){
  sammis = samplesumstats$sample_missingness_w_exclusions
  } else {
    sammis = samplesumstats$sample_missingness
  }

### feature names to exclude
if( length(qcdata$featuredata$derived_features) > 0 & derived_var_exclusion == "TRUE" ){
  w = which( qcdata$featuredata$derived_features == "yes") 
  fn2e = as.character( qcdata$featuredata$feature_names[w] )
} else {
    fn2e = NA
  }

### RUN feature summary stats funtion
featuresumstats = feature.sum.stats( wdata = qcdata$metabolitedata,
                                      sammis = sammis, 
                                      tree_cut_height = tree_cut_height,
                                      outlier_udist = outlier_udist,
                                      feature_names_2_exclude = fn2e )


## count of independent features
icount = sum(featuresumstats$table$independent_features_binary)
cat(paste0("\t\t\t- A total of ", icount ," independent features were identified in the total filtered data set.\n"))

### write feature sum stats to file
cat( paste0("\td. Writing feature summary statistics to file.\n") )

if( "featuredata" %in% names(qcdata) ){
  ## add feature stats to the feature annotation file
  # featuresumstats$table = cbind(featuresumstats$table[, 1], qcdata$featuredata, featuresumstats$table[, -1])
  featuresumstats$table = cbind( qcdata$featuredata, featuresumstats$table[, -1] )
}
n = paste0(data_dir, "metaboprep_release_", today, "/sumstats/filtered_dataset/", project, "_", today, "_feature_anno_sumstats.txt")
write.table(featuresumstats$table, file = n,
            row.names = FALSE, col.names = TRUE, 
            sep = "\t", quote = TRUE)


##################################
## C. Generation of PCs
##################################
cat( paste0("\te. Performing principle component analysis on final filtered data set.\n") )

## identify independent feature names as reported in featuresumstats
w = which(featuresumstats$table$independent_features_binary == 1)
indf = featuresumstats$table[w,1]
if( sum( indf %in% colnames(mydata$metabolitedata) ) == 0 ){
  indf = as.character( featuresumstats$table$feature_names[w] )  
}
PCs_outliers = pc.and.outliers(metabolitedata =  qcdata$metabolitedata, 
                               indfeature_names = indf)

cat( paste0("\t\t The number of informative principle components:\n") )
cat( paste0("\t\t\t 1. Cattel's Scree Test : acceleration factor = ", PCs_outliers$accelerationfactor ,"\n") )
cat( paste0("\t\t\t 1. Parrallel Analysis = ", PCs_outliers$nsig_parrallel ,"\n") )

### write sample sum stats to file
cat( paste0("\tf. Re-Writing filtered sample summary statistics to file to include PCs.\n") )

### SAMPLES
samplesumstats = cbind(samplesumstats, PCs_outliers[[1]][, 1:10] )
n = paste0(data_dir,  "metaboprep_release_", today, "/sumstats/filtered_dataset/", project, "_", today, "_sample_anno_sumstats.txt")
write.table(samplesumstats, file = n,
              row.names = FALSE, col.names = TRUE, 
              sep = "\t", quote = FALSE)

### write the variance explained by pcs out to file
cat( paste0("\tg. Writing PC statistics to file.\n\n") )

##
varexp = data.frame(VarExp = PCs_outliers[[2]])
n = paste0(data_dir, "metaboprep_release_", today, "/sumstats/filtered_dataset/", project, "_", today, "_pc_varexp.txt")
write.table(varexp, file = n,
            row.names = TRUE, col.names = TRUE, 
            sep = "\t", quote = FALSE)

n = paste0(data_dir, "metaboprep_release_", today, "/sumstats/filtered_dataset/", project, "_", today, "_featuretree.Rdata")

feature_tree = featuresumstats[[2]]
save(feature_tree, file = n)

#############################
##
## Make a FILTERED data set object
##
#############################
qc_data = list(metabolite_data = qcdata$metabolitedata,
  sample_data = samplesumstats,
  feature_data = featuresumstats$table,
  feature_tree = feature_tree,
  varexp = varexp,
  accelerationfactor = PCs_outliers$accelerationfactor,
  nparallel = PCs_outliers$nsig_parrallel
  )

#########################
##
## (IX) Save Data
##
#########################
cat( paste0("VII. Generate Data Description html report.\n") )

############
n = paste0(data_dir, "metaboprep_release_", today, "/ReportData.Rdata")
save(raw_data, qcing_data, qc_data, project, platform, data_dir, 
  feature_missingness, sample_missingness, total_peak_area_SD, PC_outlier_SD,  tree_cut_height, file = n)
############

## stop writing to log file.
sink()


#########################
##
## (X) Make Report
##
#########################
## where to print report
output_dir_path = paste0(data_dir, "metaboprep_release_", today, "/")
rdfile = paste0(output_dir_path, "ReportData.Rdata")
generate_report(full_path_2_Rdatafile = rdfile, dir_4_report = output_dir_path )

#########################
##
## (XI) Write feature distributions
##      to pdf
#########################
if(plot_feature_distributions == TRUE){
  cat( paste0("VIII. Plot feature distributions and summary statistics to pdf.\n") )
  
  feature_plots(raw_data$metabolite_data, outdir = output_dir_path)
  
  f = paste0(output_dir_path, "feature_distribution.pdf")
  cat( paste0("\t- plots for each figure written to the pdf ", f,".\n") )

}
