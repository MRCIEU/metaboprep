###########################################
## Read in Metabolon Data from their
## excel sheets.
## 
##  by: Laura Corbin & David Hughes 
##	date: May 14th 2019
###########################################

######################################
## RUN DIRECTIONS:
## - run the script by running the line
## > Rscript process_raw_data_files.R paramater_file.txt 
######################################


#######################################
##
## I) Load the MetaboQC R package
##
#######################################
library(MetaboQC)

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
    paste0("You must provide a single argument, the paramater file, when you call the script.\n\tEXAMPLE:$ Rscript --vanilla run_MetaboQC_pipeline.R parameter_file.txt.\n"),
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

## (a) What is the project name?
project <- as.character( pfile[1,2] )

## (b) What is the full path to the directory containing the DATA?	
data_dir <- as.character( pfile[2,2] )

## make sure data directory ends with a "/"
if(substring(data_dir, nchar(data_dir)) != "/"){
  data_dir = paste0(data_dir,"/")
}
# cat(paste0("I. Setting up your pipeline\n"))
# cat(paste0("\t-Your data directory is: ", data_dir, "\n"))


#######################################
##
## IV) Make a new sub directory
##
#######################################
## check for spaces in fle paths
dd = data_dir
dd = gsub(" ","\\\\ ", dd)
#cmd = paste0("mkdir -p ", data_dir, "MetaboQC_release_", today)
cmd = paste0("mkdir -p ", dd, "MetaboQC_release_", today)
system(cmd)

#######################################
##
## V) Start writing a log file
##
#######################################
logfilename = paste0(data_dir, "MetaboQC_release_", today, "/", project, "_", today,  "_logfile.txt")
sink(file = logfilename , split = TRUE  )

cat(paste0("I. Setting up your pipeline\n"))
cat(paste0("\t-Your data directory is: ", data_dir, "\n"))


#######################################
##
## III - CONT'd) Parse the info in the paramater file
##
#######################################

## (c) Name of metabolite data file
METABO_file2process = as.character( pfile[3,2] )
## identify the file type
xl_ftype = c(".xls",".xlsx", ".XLS", ".XLSX")
isexcel = sum(unlist( sapply(xl_ftype, function(x){ grep(x, METABO_file2process) } ) ) )
##
flat_ftype = c(".csv",".txt", ".tsv", ".TXT", ".CSV", ".TSV")
isflat = sum(unlist( sapply(flat_ftype, function(x){ grep(x, METABO_file2process) } ) ) )
#file2process = as.character( pfile[3,2] )
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

## (d) Name of feature annotation file
##     If you are reading in a commercial source EXCEL file 
##     can be NA. If reading in a pre-processed flat text file
##     and data is from Metabolon, it would be best to provide a 
##     column called "SUPER_PATHWAY" to identify "Xenobiotics"
FeatureAnno_file2process = as.character( pfile[4,2] )
if( !is.na(FeatureAnno_file2process)){
  cat(paste0("\t- Your provided feature annotation file to process is: ", FeatureAnno_file2process, "\n"))  
} else{
  cat(paste0("\t- You have NOT provided a feature annotation file to process.\n"))  
}

## (e) Name of sample (batch) annotation file
##     If you are reading in a commercial source EXCEL file 
##     can be NA.
SampleAnno_file2process = as.character( pfile[5,2] )
if( !is.na(SampleAnno_file2process)){
  cat(paste0("\t- You provided a sample | batch annotation file to process is: ", SampleAnno_file2process, "\n"))  
} else{
  cat(paste0("\t- You have NOT provided a sample | batch annotation file to process.\n"))  
}


## (f) What platform does your data come from?
platform = as.character( pfile[6,2] )
cat(paste0("\t- Your declared platform is: ", platform, "\n"))

## (g) QC values
feature_missingness = as.numeric( pfile[7,2] )
cat(paste0("\t- Feature filtering: Your declared feature missingness level is: ", feature_missingness, "\n"))

sample_missingness = as.numeric( pfile[8,2] )
cat(paste0("\t- Sample filtering: Your declared sample missingness level is: ", sample_missingness, "\n"))

total_peak_area_SD = as.numeric( pfile[9,2] )
cat(paste0("\t- Sample filtering: Your declared total peak area filter level, in standard deviations from the mean is: ", total_peak_area_SD, "\n"))

PC_outlier_SD = as.numeric( pfile[10,2] )
cat(paste0("\t- Sample filtering: Your declared  principal component (PC1 and PC2) exclusion, in standard deviations from the mean is: ", PC_outlier_SD, "\n\n"))


#######################################
##
## VI) Reading in the data
##
#######################################
## (a) FULL PATH TO Metabolite data file
n = paste0(data_dir, METABO_file2process)
##
if(isflat > 0){
  if( length(grep(".csv", n )) > 0 ){
    cat(paste0("\t- Reading in your csv metabolite file\n"))
    metabolitedata = read.csv(n, header = TRUE, as.is = TRUE, na.strings = c("NA","NDEF", "TAG") )
  }
  ####
  if( length( c( grep(".txt", n ), grep(".tsv", n )  ) )  > 0 ){
    cat(paste0("\t- Reading in your txt|tsv metabolite file\n"))
    metabolitedata = read.table(n, header = TRUE, as.is = TRUE, sep = "\t", na.strings = c("NA","NDEF", "TAG") )
  }
  ## remove any possible commas
  metabolitedata = apply(metabolitedata, 2, function(x){
    o = sapply(x, function(y){
      gsub(",","",y)
      })
    return(o)
    })
  metabolitedata = as.data.frame(metabolitedata)
  #########
  ## format metabolite data
  #########
  ## look to see of row names are just numerics. if yes redefine rownames and column 1 values
  editrownames = sum( rownames(metabolitedata) == 1:nrow(metabolitedata) ) /nrow(metabolitedata)
  if(editrownames == 1){
    cat(paste0("\t\t- Assuming sample IDs are in column 1 and redefining rownames\n"))
    rownames(metabolitedata) = as.character(metabolitedata[,1])
    metabolitedata = metabolitedata[,-1]
  }
  ## if platform is Nightingale edit metabolite names
  if( platform == "Nightingale"){
    cat(paste0("\t\t- Your defined platform is Nightingale,\n\t\t so editing metabolite names in an attempt to match the MetaboQC annotation file.\n"))
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

## (b) Checking for a flat text Feature Annotation File
if( !is.na(FeatureAnno_file2process) ){
  ## full path to feature annotation file
  n = paste0(data_dir, FeatureAnno_file2process)
  ##
  if( length(grep(".csv", n )) > 0 ){
    cat(paste0("\t- Reading in you csv feature annotation file\n"))
    featuredata = read.csv(n, header = TRUE, as.is = TRUE, fill = TRUE)
  }
  ####
  if( length( c( grep(".txt", n ), grep(".tsv", n )  ) ) > 0 ){
    cat(paste0("\t- Reading in you txt feature annotation file\n"))
    featuredata = read.table(n, header = TRUE, as.is = TRUE, sep = "\t", fill = TRUE)
  } 
  ## format featuredata data
  editrownames = sum( rownames(featuredata) == 1:nrow(featuredata) ) /nrow(featuredata)
  if(editrownames == 1){
    ## redefine only if the number of unique strings is the same as the number of rows
    if( length(unique(featuredata[,1])) == nrow(featuredata) ){
      cat(paste0("\t\t- Assuming feature IDs are in column 1 and redefining rownames\n"))
      rownames(featuredata) = as.character(featuredata[,1])
      #featuredata = featuredata[,-1]
    }
  }
  if( platform == "Nightingale"){
    cat(paste0("\t\t- Your defined platform is Nightingale,\n\t\t  so editing metabolite names in an attempt to match the MetaboQC annotation file.\n"))
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

## (c) Checking for a flat text Sample Annotation File
if( !is.na(SampleAnno_file2process) ){
  ## full path to feature annotation file
  n = paste0(data_dir, SampleAnno_file2process)
  ##
  if( length(grep(".csv", n )) > 0 ){
    cat(paste0("\t- Reading in you csv sample annotation file\n"))
    sampledata = read.csv(n, header = TRUE, as.is = TRUE)
  }
  ####
  if(length( c( grep(".txt", n ), grep(".tsv", n )  ) ) > 0 ){
    cat(paste0("\t- Reading in you txt sample annotation file\n"))
    sampledata = read.table(n, header = TRUE, as.is = TRUE, sep = "\t")
  } 
  ## format sampledata data
  editrownames = sum( rownames(sampledata) == 1:nrow(sampledata) ) /nrow(sampledata)
  if(editrownames == 1){
    cat(paste0("\t\t- Assuming sample IDs are in column 1 and redefining rownames\n"))
    rownames(sampledata) = as.character(sampledata[,1])
    sampledata = sampledata[,-1]
  }
}


## (d) Generate a WORKING data set for flat text source files
if(isflat > 0){
  if( !exists( x = "sampledata" ) ){
    sampledata = data.frame(SAMPLE_NAMES = rownames(metabolitedata))
  }
  ####
  if( !exists( x = "featuredata" ) ){
    featuredata = data.frame(FEATURE_NAMES = colnames(metabolitedata))
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



 
## (e) IF Data file is Excel
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
      cat( paste0("\t- Your raw Nightingale data has been read in and converted to working tab delimited text files.\n\n") )

      } 
  }

}



## READING IN DATA DONE
cat( paste0("III. Your data has been read in.\n\n") )
cat( paste0("\t-Your data has ", nrow(mydata$metabolitedata), " individuals and ", ncol(mydata$metabolitedata), " metabolites.\n\n") )
cat( paste0("\t-There are also ", ncol(mydata$sampledata), " sample annotation|batch variables.\n\n") )
cat( paste0("\t-There are also ", ncol(mydata$featuredata), " feature annotation|batch variables.\n\n") )

#########################
##
## (V)	Estimate  Summary Statistics
##
#########################
cat( paste0("IV. Estimating Summary Statistics.\n") )

##################################
## A. Summary Statistics for samples
##################################
cat( paste0("\ta. Estimating summary statistics for samples\n") )

## Is this metabolon data??
##  -- is the column SUPER_PATHWAY present in the feature data
##  -- if yes, exclude Xenobiotics from one of the missingness estimate
if( length(mydata$featuredata$SUPER_PATHWAY) > 0){
  w = which( mydata$featuredata$SUPER_PATHWAY == "Xenobiotics") 
  xeno_names = rownames(mydata$featuredata)[w]
  samplesumstats = sample.sum.stats( wdata = mydata$metabolitedata, features_names_2_exclude = xeno_names )
} else {
  samplesumstats = sample.sum.stats( wdata = mydata$metabolitedata)
}

### write sample sum stats to file
cat( paste0("\tb. Writing sample summary statistics to file.\n") )

## make a sum stats directory in data_dir
dd = data_dir
dd = gsub(" ","\\\\ ", dd)
###
cmd = paste0("mkdir -p ", dd,  "MetaboQC_release_", today, "/", "sumstats")
system(cmd)


### SAMPLES
if( "sampledata" %in% names(mydata) ){
  mydata$sampledata = cbind(mydata$sampledata, samplesumstats)
  n = paste0(data_dir,  "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_sample_anno_sumstats.txt")
  
  write.table(mydata$sampledata, file = n,
              row.names = TRUE, col.names = TRUE, 
              sep = "\t", quote = FALSE)
} else {
  n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_sample_sumstats.txt")
  write.table(samplesumstats, file = n,
              row.names = TRUE, col.names = TRUE, 
              sep = "\t", quote = FALSE)
}


##################################
## II. Summary Statistics for features
##################################
cat( paste0("\tc. Estimating summary statistics for features.\n") )

if( length(samplesumstats$sample_missingness_w_exclusions) > 0){
  featuresumstats = feature.sum.stats( wdata = mydata$metabolitedata,
                                       sammis = samplesumstats$sample_missingness_w_exclusions)
} else {
  featuresumstats = feature.sum.stats( wdata = mydata$metabolitedata,
                                       sammis = samplesumstats$sample_missingness)
}


### write feature sum stats to file
cat( paste0("\td. Writing feature summary statistics to file.\n") )

if( "featuredata" %in% names(mydata) ){
  mydata$featuredata = cbind(mydata$featuredata, featuresumstats$table)
  n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_feature_anno_sumstats.txt")
  write.table(mydata$featuredata, file = n,
              row.names = TRUE, col.names = TRUE, 
              sep = "\t", quote = TRUE)
} else {
  n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_feature_sumstats.txt")
  write.table(featuresumstats, file = n,
              row.names = TRUE, col.names = TRUE, 
              sep = "\t", quote = TRUE)
}


##################################
## III. PC outliers
##################################
cat( paste0("\te. Performing principle component analysis and identifying outliers.\n") )

## identify independent feature names as reported in featuresumstats
w = which(featuresumstats$table$independent_features_binary == 1)
indf = rownames(featuresumstats$table)[w]

PCs_outliers = pc.and.outliers(metabolitedata =  mydata$metabolitedata, 
                               indfeature_names = indf)

### write sample sum stats to file
cat( paste0("\tf. Re-Writing sample summary statistics to file to include PCs.\n") )

#samplesumstats = cbind(samplesumstats, PCs_outliers[[1]])

## make a sum stats directory in data_dir
dd = data_dir
dd = gsub(" ","\\\\ ", dd)
##
cmd = paste0("mkdir -p ", dd,  "MetaboQC_release_", today, "/", "sumstats")
system(cmd)


### SAMPLES
if( "sampledata" %in% names(mydata) ){
  mydata$sampledata = cbind(mydata$sampledata, PCs_outliers[[1]])
  n = paste0(data_dir,  "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_sample_anno_sumstats.txt")
  
  write.table(mydata$sampledata, file = n,
              row.names = TRUE, col.names = TRUE, 
              sep = "\t", quote = FALSE)
} else {
  n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_sample_sumstats.txt")
  write.table(samplesumstats, file = n,
              row.names = TRUE, col.names = TRUE, 
              sep = "\t", quote = FALSE)
}


### write the variance explained by pcs out to file
cat( paste0("\tg. Writing PC statistics to file.\n\n") )

varexp = data.frame(VarExp = PCs_outliers[[2]])
n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_pc_varexp.txt")
write.table(varexp, file = n,
            row.names = TRUE, col.names = TRUE, 
            sep = "\t", quote = FALSE)

n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_featuretree.Rdata")
feature_tree = featuresumstats[[2]]
save(feature_tree, file = n)


#########################
##
## Perform QC
##
#########################
cat( paste0("V. Performing data quality control.\n") )

dd = data_dir
dd = gsub(" ","\\\\ ", dd)
##
cmd = paste0("mkdir -p ", dd, "MetaboQC_release_", today, "/", "qc_data")
system(cmd)

### xenobiotics to exclude
w = which( mydata$featuredata$SUPER_PATHWAY == "Xenobiotics" ) 
xeno_names = rownames( mydata$featuredata )[w]

## independent features
q = which(featuresumstats$table$independent_features_binary == 1)
ind = rownames(featuresumstats$table)[q]

### execute super funciton
cat( paste0("\ta. Performing QC on data.\n") )

qcdata = perform.metabolite.qc(wdata = mydata$metabolitedata,
                               fmis = feature_missingness, 
                               smis = sample_missingness, 
                               feature_colnames_2_exclude = xeno_names, 
                               tpa_out_SD = total_peak_area_SD, 
                               PC_out_SD = PC_outlier_SD,
                               ind_feature_names = ind )

## write QC data to file
cat( paste0("\tb. Writing QC data to file.\n\n") )

n = paste0(data_dir, "MetaboQC_release_", today, "/qc_data/", project, "_", today, "_QCd_data.txt")
write.table(qcdata, file = n,
            row.names = TRUE, col.names = TRUE, 
            sep = "\t", quote = FALSE)

  
  



# #########################
# ## 	Make QC Report
# #########################
# if(  length( grep("Nightingale", metabo_out_name) ) > 0 ){
#   cat( paste0("--- Performing Nightingale QC analysis and making a pdf report.\n") )
#   #rmarkdown::render("Nightingale_QC_Report.Rmd")
# } else {
#   if( length( grep("Metabolon", metabo_out_name) ) > 0 ){
#     cat( paste0("--- Performing Metabolon QC analysis and making a pdf report.\n") )
#     #rmarkdown::render("Metabolon_QC_Report.Rmd")
#   }
# }
# 


cat( paste0("VI. Generate Data Description pdf report.\n") )

## write data need by Rmarkdown to an Rdata object
metdata = mydata$metabolitedata
sdata = mydata$sampledata
fdata = mydata$featuredata
qdata = qcdata
varexp = varexp
ftree = feature_tree
project = project
platform = platform
data_dir = data_dir   # paste0(data_dir, "MetaboQC_release_", today, "/sumstats/")

############
n = paste0(data_dir, "MetaboQC_release_", today, "/ReportData.Rdata")
save(metdata, sdata, fdata, qdata, varexp, ftree, project, platform, data_dir, file = n)
############

#cannot open compressed file '/Volumes/metQC/recall/MetaboQC_release_2019_09_02/sumstats/MetaboQC_release_2019_09_02/ReportData.Rdata', probable reason 'No such file or directory'

## stop writing to log file.
 sink()

### MAKE REPORT !
output_dir_path = paste0(data_dir, "MetaboQC_release_", today, "/")

invisible( 
rmarkdown::render(paste0("QC_Report.Rmd"), 
  output_dir = output_dir_path, 
  output_file = "Project_Data_Report.pdf" , params = list(Rdatafile = n, out_dir = output_dir_path)
  )
)
  
