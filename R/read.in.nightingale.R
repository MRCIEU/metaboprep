#' read in Nightingale Health metabolomics data
#'
#' This function reads in a Nightingale raw data excel file, writes the (1) metabolite, (2) sample annotation, and (3) feature annotation data to flat text files. It also returns a list object of the same data.
#'
#' @param file2process the name of the xls file to process
#' @param data_dir the full path to the directory holding your Nightingale excel file
#' @param projectname a name for your project
#'
#' @keywords Nigtingale Health meatbolomics
#' 
#' @importFrom utils write.table
#' 
#' @return a list object of (1) metabolite, (2) sample annotation, and (3) feature annotation data
#'
#' @export
#'
#' @examples
#' read.in.nightingale(file2process = "NH_data_release.xls", 
#'  data_dir = "/File/sits/here/", 
#'  projectname = "My Amazing Project")
#'
read.in.nightingale = function( file2process, data_dir, projectname ){

  ## Grab current data
  today = Sys.Date()
  today = gsub("-","_",today)
  
  ## make sure data directory ends with a "/"
  if(substring(data_dir, nchar(data_dir)) != "/"){
    data_dir = paste0(data_dir,"/")
  }

  ## Make a new sub directory
  cmd = paste0("mkdir -p ", data_dir, "metaboprep_release_", today)
  system(cmd)

  
  ### Is the data file an excel sheet or a flat text file ??
  ftype = c(".xls",".xlsx", ".XLS", ".XLSX")
  isexcel = sum(unlist( sapply(ftype, function(x){ grep(x, file2process) } ) ) )
  
  ## check that you passed an excel sheet
  if( isexcel < 1 ){
    stop( 
      paste0("You must provide the name of the commercial Nightingale excel sheet to process your data using this function.\n"),
      call.=FALSE)
  } else {

    ### it is an excel sheet so read in with readxl
    cat(paste0("\t- Your File Being Processed is: ", file2process, "\n"))
    n = paste0(data_dir, file2process)

    ## read xls sheet names
    sheetnames = readxl::excel_sheets( n )

    ## Progress Statements
    cat(paste0("\t- There is/are ", length(sheetnames), " sheet(s) in your excel file\n"))
    cat( paste0("\t\tThey are:\n") )
    cat( paste0("\t\t\t- ", sheetnames, "\n") )
    cat( paste0("\t- You have declared that the data being processed is from Nightingale\n") )
    cat( paste0("\t- Reading in your metabolite data\n") )

    #########################
    ## TASK 1
    ## read in the data
    #########################
    ## Nigtingales Data is provided on sheet 1, of the excel sheet.
    ## We are assuming this remains true here.
    d = readxl::read_excel( n , sheet = 1, na = c("NA","NDEF", "TAG") )
    d = as.data.frame(d)

    #########################
    ## TASK 2
    ## remove INFORMATION row
    #########################
    ## remove infomation rows
    count = apply(d, 1, function(x){sum(is.na(x))})
    r = which(count > ncol(d)*0.8)
    d = d[-r, ]

    #########################
    ## TASK 3
    ## Identify Sample FLAGS
    #########################
    ## ARE THERE FLAG COLUMNS at the start of the sheet ???
    countNAs = apply(d, 2, function(x){sum(is.na(x))})
    w_NAs = which(countNAs > nrow(d)*0.5)
    ##
    countX = apply(d, 2, function(x){ sum( x == "X", na.rm = TRUE) })
    w_X = which( countX > 1 )
    ##
    r = w_X[w_X %in% w_NAs]
    ##
    if(length(r) > 0){
      flags = data.frame( d[ , r] )
      flagnames = as.character( flags[1, ] )
      #cn = as.character( flags[1,] )
      #flags = data.frame( flags[ -1, ] )
      #colnames(flags) = cn
      ## edit d to remove flags
      d = d[ , -r]
    }

    #########################
    ## TASK 4
    ## Sample Starting Place
    #########################
    ## find where sample data starts?
    ## relying on the location of "success %" in column 1
    ## to determine where samples start
    w = which( sapply( d[,1], function(x){ grep("success", x) }) == 1 )
    w = w+1
    metabolite_data = d[w:nrow(d), ]
    colnames(metabolite_data) = paste( d[2,] )
    
    ## DEFINE FLAGS
    flags = data.frame( flags[ w:nrow(flags), ] )
    colnames(flags) = flagnames

    #########################
    ## TASK 4
    ## Edit colimun metabolite names
    #########################
    ## this aids matching to ng_anno object    
    colnames(metabolite_data) = gsub("_.", "pct", colnames(metabolite_data))
    colnames(metabolite_data) = gsub("%", "pct", colnames(metabolite_data))
    colnames(metabolite_data) = gsub("/", "_", colnames(metabolite_data))
    colnames(metabolite_data) = gsub("\\.", "", colnames(metabolite_data))
    colnames(metabolite_data) = gsub("-", "", colnames(metabolite_data))
    colnames(metabolite_data) = gsub("_", "", colnames(metabolite_data))
    colnames(metabolite_data) = tolower(colnames(metabolite_data))

    #########################
    ## TASK 5
    ## Data cleaning
    #########################
    ## are there any empty data rows at the bottom of the data frame?
    count = apply(metabolite_data, 1, function(x){sum(is.na(x))})
    r = which(count == ncol(metabolite_data))
    if(length(r) > 0){
      metabolite_data = metabolite_data[-r, ]
      flags = data.frame( flags[-r, ] )
      colnames(flags) = flagnames
    }

    ### add rownames
    rownames(metabolite_data) = metabolite_data[, 1 ]
    rownames(flags) = metabolite_data[, 1 ]
    metabolite_data = metabolite_data[, -1]

    ## insure values are numeric
    n = rownames(metabolite_data)
    metabolite_data = apply(metabolite_data, 2, function(x){ as.numeric(as.character(x)) })
    rownames(metabolite_data) = n

    # ## add sampleids to flag data if it is present
    # if( exists("flags") == TRUE){
    #   flags = flags[w+1:nrow(flags), ]
    #   if(length(r) > 0){
    #     flags = flags[-r, ]
    #   }
    #   rownames(flags) = rownames(metabolite_data)
    # }

    #########################
    ## TASK 6
    ## Metadata (samples)
    #########################
    ### is there a metadata excel sheet in the directory ??
    fnames = list.files(data_dir)
    w = grep("metadata", tolower( fnames ) )
    # metadatafile2process = c( gsub("Results", "Metadata", file2process) )
    metadatafile2process = fnames[w]

    ## if the metadata file exists, read in and process
    if(metadatafile2process %in% fnames){
      n = paste0(data_dir, metadatafile2process)
      metadata = as.data.frame( readxl::read_excel( n , sheet = 1, na = c("NA","NDEF", "TAG") ) )
      colnames(metadata) = gsub(" " , "_", colnames(metadata))
      colnames(metadata) = gsub("/" , "_", colnames(metadata))
      colnames(metadata) = gsub("-" , "_", colnames(metadata))
      colnames(metadata) = gsub("__" , "_", colnames(metadata))
      colnames(metadata) = gsub("__" , "_", colnames(metadata))
      ### THIS IS A HUGE ASSUMPTION !!! 
      ### ASSUMING THAT THE SAMPLE IDs ARE
      ### IN COLUMN 1
      ### ** Problem: ids names can be Sample_ID, SampleID, Client_ID or ClientID
      rownames(metadata) = metadata[,1]
      ###
      ## order the metadata to match the metabolite and flag data
      # if(length(metadata$Sample_ID) > 0){
      #   m = match(rownames(metabolite_data), metadata$Sample_ID)
      # } else {
      #   m = match(rownames(metabolite_data), metadata$Client_ID)
      # }
      m = match(rownames(metabolite_data), rownames(metadata) )
      metadata = metadata[m, ]
      # rownames(metadata) = rownames(metabolite_data)
      cat( paste0("\t- Your data contains ", ncol(metadata), " metadata points that you should be aware of.\n") )
    }

    if( exists("flags") & exists("metadata")  ){
      cat( paste0("\t- Your data contains ", ncol(flags), " flags for unique features that you should carefully evaluate.\n") )
      cat( paste0("\t- Both metadata and flags are being merged together into a single tab-delm text file.\n") )
      ##
      metadata = cbind(metadata, flags)
      rm(flags)
    } else {
      cat( paste0("\t- Your data contains no flags | warnings for unique features.\n") )
      }

    #########################
    ## TASK 7
    ## Simple Summary
    #########################
    ###  Summary data for the user
    cat( paste0("\t- Your data contains ", nrow(metabolite_data),  " samples.\n") )
    cat( paste0("\t- Your data contains ", ncol(metabolite_data),  " features\n") )

    #########################
    ## TASK 8
    ## write data to file
    #########################
    dd = data_dir
    dd = gsub(" ","\\\\ ", dd)
    ###
    cmd = paste0("mkdir -p ", dd,  "metaboprep_release_", today, "/", "raw_data")
    system(cmd)

    ## (1) Write abundance data to file
    #metabo_out_name = paste0(data_dir, projectname, "_", today, "_Nightingale_metabolitedata.txt")
    metabo_out_name = paste0(data_dir,  "metaboprep_release_", today, "/raw_data/", projectname, "_", today, "_Nightingale_metabolitedata.txt")

    cat( paste0("\t- Writing your metabolite data set to the tab-delmited text file ", metabo_out_name,  "\n") )
    utils::write.table(metabolite_data, file = metabo_out_name, row.names = TRUE,
                col.names = TRUE, sep = "\t", quote = FALSE)

    ## (2) Write Meta Data to file
    if( exists("metadata") == TRUE){
      cat( paste0("\t- Your data contains a metadata excel file.\n") )
      ##
      #sampledata_out_name = paste0(data_dir, projectname, "_",today,"_Nightingale_sampledata.txt")
      sampledata_out_name = paste0(data_dir,  "metaboprep_release_", today, "/raw_data/", projectname, "_",today,"_Nightingale_sampledata.txt")
      ##
      cat( paste0("\t- Writing your metadata to the tab-delmited text file ", sampledata_out_name,  "\n") )
      utils::write.table(metadata, file = sampledata_out_name, row.names = FALSE,
                  col.names = TRUE, sep = "\t", quote = TRUE)
    }

    ## (3) FEATURE DATA
    ### make a feature/feature annotation sheet
    ### using Nightingale information on matabolites that we are providing
    ### this metabolite data sheet may become outdated at some point as more features are added by
    ### Nightingale
    feature_names = colnames(metabolite_data)
    m = match(feature_names,  ng_anno[,1])
    featuresheet = cbind( feature_names, ng_anno[m, ] )
    cat( paste0("\t- A feature annotation file is being generated.\n") )
    ##
    #featuredata_out_name = paste0(data_dir, projectname,"_",today, "_Nightingale_featuredata.txt")
    featuredata_out_name = paste0(data_dir,  "metaboprep_release_", today, "/raw_data/", projectname,"_",today, "_Nightingale_featuredata.txt")
    
    
    cat( paste0("\t- Writing your feature annotation to the tab-delmited text file ", featuredata_out_name,  "\n") )
    utils::write.table(featuresheet, file = featuredata_out_name, row.names = FALSE,
                col.names = TRUE, sep = "\t", quote = TRUE)
    

    #########################
    ## TASK 9
    ## make a list of objects
    #########################
    ## return data to user in R session

    mydata = list(metabolitedata = metabolite_data, sampledata = metadata, featuredata = featuresheet)
    return(mydata)

  }## END OF ELSE STATEMENT THAT EXCEL SHEET IS BEING PROVIDED

}  ## END OF FUNCTION




