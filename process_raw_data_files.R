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
## I) Load the MetaboQC R package
#######################################
library(MetaboQC)

#######################################
## II) Read in arguments from command line
#######################################
## You should pass a single argument
## that is the path to the paramater_file.txt
args = commandArgs(trailingOnly=TRUE)

## Check that you passed an argument to the script
if(length(args) != 1){
  stop( 
    paste0("You must provide a single argument, the paramater file, when you call the script.\n\tEXAMPLE:$ Rscript --vanilla process_raw_data_files.R parameter_file.txt.\n"),
    call.=FALSE)
} 

## record todays date
today = Sys.Date()
today = gsub("-","_",today)

#######################################
## III) Parse the info in the paramater file
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
cat(paste0("--- Your data directory is: ", data_dir, "\n"))

## (c) Name of metabolite data file
METABO_file2process = as.character( pfile[3,2] )
## identify the file type
xl_ftype = c(".xls",".xlsx", ".XLS", ".XLSX")
isexcel = sum(unlist( sapply(xl_ftype, function(x){ grep(x, METABO_file2process) } ) ) )
##
flat_ftype = c(".csv",".txt", ".TXT", ".CSV")
isflat = sum(unlist( sapply(flat_ftype, function(x){ grep(x, METABO_file2process) } ) ) )
#file2process = as.character( pfile[3,2] )
cat(paste0("--- Your metabolite data file is: ", METABO_file2process, "\n"))
if(isexcel > 0){
  cat(paste0("\tYour metabolite data file was identified as an excel sheet\n\tand will be processed as a commercial source file.\n"))  
  } else {
    if(isflat > 0){
      cat(paste0("\tYour metabolite data file was identified as a previously processed flat text file.\n"))    
    } else {
      cat( paste0("\tUnable to identify the type of file you provided.\n\tPlease be sure it is an xls, xlsx, txt, or csv.\n") )    
    } 
  }

## (d) Name of feature annotation file
##     If you are reading in a commercial source EXCEL file 
##     can be NA. If reading in a pre-processed flat text file
##     and data is from Metabolon, it would be best to provide a 
##     column called "SUPER_PATHWAY" to identify "Xenobiotics"
FeatureAnno_file2process = as.character( pfile[4,2] )
if( !is.na(FeatureAnno_file2process)){
  cat(paste0("--- You provided a feature annotation file to process. It is: ", FeatureAnno_file2process, "\n"))  
}

## (e) Name of sample (batch) annotation file
##     If you are reading in a commercial source EXCEL file 
##     can be NA.
SampleAnno_file2process = as.character( pfile[5,2] )
if( !is.na(SampleAnno_file2process)){
  cat(paste0("--- You provided a sample | batch annotation file to process. It is: ", SampleAnno_file2process, "\n"))  
}

## (f) What platform does your data come from?
platform = as.character( pfile[6,2] )
cat(paste0("--- Your Declared platform is: ", platform, "\n"))

## (g) QC values
feature_missingness = as.numeric( pfile[7,2] )
sample_missingness = as.numeric( pfile[8,2] )
total_peak_area_SD = as.numeric( pfile[9,2] )
PC_outlier_SD = as.numeric( pfile[10,2] )


#######################################
## IV) Make a new sub directory
#######################################
cmd = paste0("mkdir -p ", data_dir, "MetaboQC_release_", today)
system(cmd)

#######################################
## V) Start writing a log file
#######################################
logfilename = paste0(data_dir, "MetaboQC_release_", today, "/", project, "_", today,  "_logfile.txt")
sink(file = logfilename , split = TRUE  )


#############################
### 
### Process if Nightingale
###
#############################
#if(platform == "Nightingale"){
#  cat( paste0("--- I. Processing your Nightingale data.\n") )
#  ## Read in the raw data, excel files, write to flat text in the data directory and return all data as a list
#  mydata = read.in.nightingale( file2process = file2process, data_dir = data_dir,  projectname = project )
#}

if(platform == "Nightingale"){
  cat( paste0("--- I. Processing your Nightingale data.\n") )
  if( !is.na(pfile[3,2]) ){
      ## Read in the raw data, excel files, write to flat text in the data directory and return all data as a list
      mydata = read.in.nightingale( file2process = file2process, data_dir = data_dir, projectname = project )
    } 
}


#############################
###
### Process if Metabolon
###
#############################
if(platform == "Metabolon"){
  cat( paste0("--- I. Processing your Metabolon data.\n") )
  if( !is.na(pfile[3,2]) ){
    ## Read in the raw data, excel files, write to flat text in the data directory and return all data as a list
    mydata = read.in.metabolon( file2process = file2process, data_dir = data_dir, projectname = project )
    } else {  ### Jan 24th: New additions made here to accomodate BiB (Born in Bradford) data set. 
      ## Make a new sub directory
      cmd = paste0("mkdir ", data_dir, "MetaboQC_release_", today)
      system(cmd)

      ## Read in data from a data file that IS NOT the "original" EXCEL SHEET from Metabolon
      n = paste0(data_dir, file2process)
      if(grep("csv", n) == 1){
        primary_metabolitedata = read.csv(n, header = TRUE, as.is = TRUE, row.names = 1)  
      } else {  ## assume to be tab delimited
        primary_metabolitedata = read.table(n, header = TRUE, as.is = TRUE, sep = "\t", row.names = 1)  
      }
      ##### Feature Annotation
      n = paste0(data_dir, annofile2process)
      if(grep("csv", n) == 1){
        featuresheet = read.csv(n, header = TRUE, as.is = TRUE, row.names = 1)  
      } else {  ## assume to be tab delimited
        featuresheet = read.table(n, header = TRUE, as.is = TRUE, sep = "\t", row.names = 1)  
      }
      colnames(featuresheet) = toupper( colnames(featuresheet) )
      #######
      sampledata = data.frame(SAMPLE_NAME = rownames(primary_metabolitedata))
      #######
      mydata = list(metabolitedata = primary_metabolitedata, sampledata = sampledata,  featuredata = featuresheet )
    }
}


#########################
## READING IN DATA DONE
#########################
cat( paste0("--- Your raw data has been read in and converted to working tab delimited text files.\n\n") )


#########################
## 	Estimate 
##  Summary Statistics
#########################
cat( paste0("--- II. Estimating Summary Statistics.\n") )

##################################
## I. Summary Statistics for samples
##################################
cat( paste0("----- IIa. Estimating summary statistics for samples\n") )

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
cat( paste0("----- IIb. Writing sample summary statistics to file.\n") )

## make a sum stats directory in data_dir
cmd = paste0("mkdir ", data_dir,  "MetaboQC_release_", today, "/", "sumstats")
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
cat( paste0("----- IIc. Estimating summary statistics for features.\n") )

if( length(samplesumstats$sample_missingness_w_exclusions) > 0){
  featuresumstats = feature.sum.stats( wdata = mydata$metabolitedata,
                                       sammis = samplesumstats$sample_missingness_w_exclusions)
} else {
  featuresumstats = feature.sum.stats( wdata = mydata$metabolitedata,
                                       sammis = samplesumstats$sample_missingness)
}


### write feature sum stats to file
cat( paste0("----- IId. Writing feature summary statistics to file.\n") )

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
cat( paste0("----- IIe. Performing principle component analysis and identifying outliers.\n") )

## identify independent feature names as reported in featuresumstats
w = which(featuresumstats$table$independent_features_binary == 1)
indf = rownames(featuresumstats$table)[w]

PCs_outliers = pc.and.outliers(metabolitedata =  mydata$metabolitedata, 
                               indfeature_names = indf)

# #### TESTING AREA
# f = "/Volumes/metQC/fgfp/MetaboQC_release_2019_Aug_31/fgfp_2019_Aug_31_OrigScale_Metabolon_metabolitedata.txt"
# met_test = read.table(f, header = TRUE, sep = "\t", as.is = TRUE)

# f = "/Volumes/metQC/fgfp/MetaboQC_release_2019_Aug_31/sumstats/fgfp_2019_Aug_31_feature_anno_sumstats.txt"
# feature_test = read.table(f, header = TRUE, sep = "\t", as.is = TRUE)

# w = which(feature_test$independent_features_binary == 1)
# indf = rownames(feature_test)[w]

# PCs_outliers = pc.and.outliers(metabolitedata =  met_test, 
#                                indfeature_names = indf)
# ###### END OF TESTING AREA


### write sample sum stats to file
cat( paste0("----- IIf. Re-Writing sample summary statistics to file to include PCs.\n") )

#samplesumstats = cbind(samplesumstats, PCs_outliers[[1]])

## make a sum stats directory in data_dir
cmd = paste0("mkdir ", data_dir,  "MetaboQC_release_", today, "/", "sumstats")
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
cat( paste0("----- IIg. Writing PC statistics to file.\n") )

varexp = data.frame(VarExp = PCs_outliers[[2]])
n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_pc_varexp.txt")
write.table(varexp, file = n,
            row.names = TRUE, col.names = TRUE, 
            sep = "\t", quote = FALSE)

n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_featuretree.Rdata")
feature_tree = featuresumstats[[2]]
save(feature_tree, file = n)



##################################
### II. Print SumStats to file
### using data directory ripped
### from inputting file name
###################################
# cat( paste0("----- IId. Writing Summary Statistics to file.\n") )

# ## make a sum stats directory in data_dir
# cmd = paste0("mkdir ", data_dir,  "MetaboQC_release_", today, "/", "sumstats")
# system(cmd)


# ### SAMPLES
# if( "sampledata" %in% names(mydata) ){
#   mydata$sampledata = cbind(mydata$sampledata, samplesumstats)
#   n = paste0(data_dir,  "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_sample_anno_sumstats.txt")
  
#   write.table(mydata$sampledata, file = n,
#               row.names = TRUE, col.names = TRUE, 
#               sep = "\t", quote = FALSE)
# } else {
#   n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_sample_sumstats.txt")
#   write.table(samplesumstats, file = n,
#               row.names = TRUE, col.names = TRUE, 
#               sep = "\t", quote = FALSE)
# }


# ### FEATURES
# if( "featuredata" %in% names(mydata) ){
#   mydata$featuredata = cbind(mydata$featuredata, featuresumstats$table)
#   n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_feature_anno_sumstats.txt")
#   write.table(mydata$featuredata, file = n,
#               row.names = TRUE, col.names = TRUE, 
#               sep = "\t", quote = TRUE)
# } else {
#   n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_feature_sumstats.txt")
#   write.table(featuresumstats, file = n,
#               row.names = TRUE, col.names = TRUE, 
#               sep = "\t", quote = TRUE)
# }

# ### write the variance explained by pcs out to file
# varexp = data.frame(VarExp = PCs_outliers[[2]])
# n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_pc_varexp.txt")
# write.table(varexp, file = n,
#             row.names = TRUE, col.names = TRUE, 
#             sep = "\t", quote = FALSE)

# n = paste0(data_dir, "MetaboQC_release_", today, "/sumstats/", project, "_", today, "_featuretree.Rdata")
# feature_tree = featuresumstats[[2]]
# save(feature_tree, file = n)


#########################
##
## Perform QC
##
##
#########################
cat( paste0("--- III. Performing data quality control.\n") )

cmd = paste0("mkdir ", data_dir, "MetaboQC_release_", today, "/", "qc_data")
system(cmd)

### xenobiotics to exclude
w = which( mydata$featuredata$SUPER_PATHWAY == "Xenobiotics" ) 
xeno_names = rownames( mydata$featuredata )[w]

## independent features
q = which(featuresumstats$table$independent_features_binary == 1)
ind = rownames(featuresumstats$table)[q]

### execute super funciton
cat( paste0("----- IIIa. Performing QC on data.\n") )

qcdata = perform.metabolite.qc(wdata = mydata$metabolitedata,
                               fmis = feature_missingness, 
                               smis = sample_missingness, 
                               feature_colnames_2_exclude = xeno_names, 
                               tpa_out_SD = total_peak_area_SD, 
                               PC_out_SD = PC_outlier_SD,
                               ind_feature_names = ind )

## write QC data to file
cat( paste0("----- IIIb. Writing QC data to file.\n") )

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


cat( paste0("--- Generate Data Description pdf report.\n") )

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

rmarkdown::render(paste0("QC_Report.Rmd"), 
  output_dir = output_dir_path, 
  output_file = "Project_Data_Report.pdf" , params = list(Rdatafile = n, out_dir = output_dir_path)
  )

  
