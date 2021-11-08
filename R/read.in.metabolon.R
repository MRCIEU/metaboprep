#' A Function to read in Metabolon excel sheets
#'
#' This function allows you to read in Metabolon raw data and push out flat text files
#' @param file2process the name of the xls file to process
#' @param data_dir the full path to the directory holding your Metabolon excel file
#' @param projectname a name for your project
#' @keywords metabolon
#' @export
#' @examples
#' read.in.metabolon()
read.in.metabolon = function( file2process, data_dir, projectname ){
  
  ## Grab current data
  today = Sys.Date()
  today = gsub("-","_",today)
  
  ## make sure data directory ends with a "/"
  if(substring(data_dir, nchar(data_dir)) != "/"){
    data_dir = paste0(data_dir,"/")
  }

  ## Make a new sub directory
  dd = data_dir
  dd = gsub(" ","\\\\ ", dd)
  data_dir = dd
  ###
  cmd = paste0("mkdir -p ", data_dir, "metaboprep_release_", today)
  system(cmd)

  ###  Raw data directory
  cmd = paste0("mkdir -p ", data_dir,  "metaboprep_release_", today, "/", "raw_data")
  system(cmd)


  
  ### Is the data file an excel sheet or a flat text file ??
  ftype = c(".xls",".xlsx", ".XLS", ".XLSX")
  isexcel = sum(unlist( sapply(ftype, function(x){ grep(x, file2process) } ) ) )
  
  ## check that you passed an excel sheet
  if( isexcel < 1 ){
    stop( 
      paste0("You must provide the name of the commercial Metabolon excel sheet to process your data using this function.\n"),
      call.=FALSE)
  } else {
    
    ### it is an excel sheet so read in with readxl
    cat(paste0("\t- Your File Being Processed is: ", file2process, "\n"))
    n = paste0(data_dir, file2process)
    
    ## read xls sheet names
    sheetnames = readxl::excel_sheets( n )
    
    
    cat(paste0("\t- There is/are ", length(sheetnames), " sheet(s) in your excel file\n"))
    cat( paste0("\t\t- They are:\n") )
    cat( paste0("\t\t\t- ", sheetnames, "\n") )
    cat( paste0("\t- You have declared that the data being processed is from Metabolon\n") )
    cat( paste0("\t- Reading in your metabolite data\n") )
    
    #########################
    ## TASK 1
    ## read in the first data sheet
    #########################
    ## Progress Statement
    cat( paste0("\t- Reading in sheet 2\n") )
    d = readxl::read_excel( n , sheet = 2, na = c("NA","NDEF", "TAG") )
    d = as.data.frame(d)

    #########################
    ## TASK 2
    ## Process sample data
    #########################
    ## Progress Statement
    cat( paste0("\t- Processing Sample Data\n") )
    
    ## Make a sampledata dataframe
    blankrows = which( is.na(d[,2]) )  ## how many rows have no data in the excel sheet
    blankcols = which( is.na(d[1,]) ) ## how many columns have no data in the excel sheet
    
    ## filter the blankrows that happen to be NA but are among the first rows in the sheet
    w = which( ( blankrows[-1] - blankrows[-length(blankrows)]) > 1 )
    if(length(w)>0){
      blankrows = blankrows[ 1:w ]  ## only the early rows are to be kept
    }

    a = blankrows  ## rows containing sample and batch data
    b = (blankcols[length(blankcols)] + 1):ncol(d) ## the first column containing data
    
    samplesheet = t(d[ a,b ])
    samplesheet = cbind(rownames(samplesheet), samplesheet) ## add rownames (sample.names) to the samplesheet
    colnames(samplesheet) = gsub(" ","_",samplesheet[1,]) ## reasign column names and substitute spaces for "_"
    samplesheet = samplesheet[-1,]
    
    ## write table to file
    sampledata_out_name = paste0(data_dir, "metaboprep_release_", today, "/raw_data/", project, "_", today,  "_Metabolon_sampledata.txt")
    write.table(samplesheet, file = sampledata_out_name, row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    
    #########################
    ## TASK 3
    ##  process metabolite anno
    ##  data
    #########################
    ## Progress Statement
    cat( paste0("\t- Processing Metabolite annotation data\n") )
    
    ## Make a meta-data sheet about the metabolites
    a = (blankrows[length(blankrows)] + 1):nrow(d)
    b = 1:(blankcols[length(blankcols)] + 1)
    featuresheet = d[a,b]
    ### column names
    cnames = as.character(unlist( featuresheet[1,] )); cnames = gsub("  ","", cnames)
    cnames = gsub(" ","_", cnames)
    w = grep("Group", cnames); cnames[w] = "HMDB"
    colnames(featuresheet) = cnames
    featuresheet = featuresheet[-1,]
    
    ## add row names to file
    rownames(featuresheet) = paste0("compid_",featuresheet$COMP_ID)
    featuresheet = cbind(feature_names = rownames(featuresheet), featuresheet)

    ## write table to file
    featuredata_out_name = paste0(data_dir, "metaboprep_release_", today, "/raw_data/", project, "_",today, "_Metabolon_featuredata.txt")
    write.table(featuresheet, file = featuredata_out_name, row.names = TRUE, col.names = TRUE, sep = "\t", quote = TRUE)
    
    #########################
    ## TASK 4
    ##  Process Metabolite Data
    ##  on tab 2  
    ##  ASSUMED to be Original and Raw
    #########################
    ## Progress Statement
    cat( paste0("\t- Processing Metabolite data on sheet 2: ", sheetnames[2],"\n") )
    
    ## Output the Original Scale metabolite data
    a = (blankrows[length(blankrows)] + 2):nrow(d)
    b = (blankcols[length(blankcols)] + 2):ncol(d)
    metabolitedata = d[a,b]
    metabolitedata[metabolitedata == ""] = NA ## turn "" into NAs
    ## remove all commas
    for(i in 1:ncol(metabolitedata)){
      metabolitedata[,i] = as.numeric( sapply(metabolitedata[,i], function(x){ gsub(",","",x) }) )
    }
    ## Transpose and redefine column (metabolite) names to COMPIDs
    metabolitedata = t(metabolitedata)
    colnames(metabolitedata) = paste0("compid_",featuresheet$COMP_ID)
    
    ## write table to file
    metabo_out_name = paste0(data_dir, "metaboprep_release_", today, "/raw_data/", project,"_", today, "_", sheetnames[2], "_Metabolon_metabolitedata.txt")
    write.table(metabolitedata, file = metabo_out_name, row.names = TRUE, col.names = TRUE, sep = "\t", quote = TRUE)
    
    ## save primary raw metabolite data to a new element
    primary_metabolitedata = metabolitedata
    rm(metabolitedata)
    #########################
    ## TASK 5
    ##  Define mydata
    #########################
    mydata = list(metabolitedata = primary_metabolitedata, sampledata = samplesheet, featuredata = featuresheet )
    
    #########################
    ## TASK 6
    ##  Process Metabolite Data
    ##  on all other tabs
    #########################
    for(i in 3:length(sheetnames)){
      ## Progress Statement
      cat( paste0("\t- Processing Metabolite data on sheet ",i, ": ", sheetnames[i],"\n") )
      
      d = readxl::read_excel( n , sheet = i, na = c("NA","NDEF", "TAG") )
      d = as.data.frame(d)
      ###############################################
      ### re-estimate the number of blank rows
      ### in case it varies from sheet 1
      ###############July 29th addition##############
      blankrows = which( is.na(d[,2]) )  ## how many rows have no data in the excel sheet
      blankcols = which( is.na(d[1,]) ) ## how many columns have no data in the excel sheet
      
      ## filter the blankrows that happen to be NA but are among the first rows in the sheet
      w = which( ( blankrows[-1] - blankrows[-length(blankrows)]) > 1 )
      if(length(w)>0){
        blankrows = blankrows[ 1:w ]  ## only the early rows are to be kept
      }
      ###########End of July 29th addition###############
      ####
      a = (blankrows[length(blankrows)] + 2):nrow(d)
      b = (blankcols[length(blankcols)] + 2):ncol(d)
      metabolitedata = d[a,b]
      metabolitedata[metabolitedata == ""] = NA ## turn "" into NAs
      ## remove all commas
      for(j in 1:ncol(metabolitedata)){
        metabolitedata[,j] = as.numeric( sapply(metabolitedata[,j], function(x){ gsub(",","",x) }) )
      }
      ## Transpose and redefine column (metabolite) names to COMPIDs
      metabolitedata = t(metabolitedata)
      colnames(metabolitedata) = paste0("compid_",featuresheet$COMP_ID)
      
      ## add data to mydata
      mydata[[sheetnames[i]]] = metabolitedata

      ## write table to file
      outname = paste0(data_dir, "metaboprep_release_", today, "/raw_data/", project,"_", today, "_", sheetnames[i], "_Metabolon_metabolitedata.txt")
      write.table(metabolitedata, file = outname, row.names = TRUE, col.names = TRUE, sep = "\t", quote = FALSE)
    }
    
    #########################
    ## TASK 7
    ## return data to user
    ######################### 
    return(mydata)
    
  }  ### END OF ELSE STATAMENT, yes you passed the function an excel sheet
  
} ### END OF READ IN RAW METABOLON DATA FUNCTION


