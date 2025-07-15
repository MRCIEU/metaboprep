#' @title Read Metabolon Data
#' @param filepath character, commercial Metabolon excel sheet with extension .xls or .xlsx
#' @param sheet character or integer, the excel sheet name (or index) from which to read. 
#' @param feature_sheet character or integer, the excel sheet name (or index) from which to read the feature data. 
#' @param feature_id_col character, the excel column containing the feature_id mapping to the data. 
#' @param sample_sheet character or integer, the excel sheet name (or index) from which to read the sample data. 
#' @param sample_id_col character, the excel column containing the sample_id mapping to the data. 
#' @returns list,  list(data = matrix, samples = samples data.frame, features = features data.frame)
#'
#' @examples
#' # version 1 data format
#' filepath1 <- system.file("extdata", "metabolon_v1_example.xlsx", package = "metaboprep")
#' m <- read_metabolon(filepath1, sheet = 2)
#' 
#' # version 2 data format
#' filepath2 <- system.file("extdata", "metabolon_v2_example.xlsx", package = "metaboprep")
#' m <- read_metabolon(filepath2, sheet = 'OrigScale')
#'
#' 
#' # version 3 data format
#' filepath3 <- system.file("extdata", "metabolon_v3_example.xlsx", package = "metaboprep")
#' m <- read_metabolon(filepath3, 
#'                     sheet = 'Batch-normalized Data', 
#'                     feature_sheet = 'Chemical Annotation', 
#'                     feature_id_col = 'CHEM_ID', 
#'                     sample_sheet = 'Sample Meta Data', 
#'                     sample_id_col = 'PARENT_SAMPLE_NAME')
#'
#' @importFrom readxl excel_sheets read_xlsx
#' @export
read_metabolon <- function(filepath, sheet = NULL, feature_sheet = NULL, feature_id_col = NULL, sample_sheet = NULL, sample_id_col = NULL) {

  # testing ====
  if (FALSE) {
    #filepath <- system.file("extdata", "metabolon_v1_example.xlsx", package = "metaboprep")
    filepath <- system.file("extdata", "metabolon_v2_example.xlsx", package = "metaboprep")
    sheet = 'OrigScale'
    feature_sheet = NULL
    feature_id_col = NULL
    sample_sheet = NULL
    sample_id_col = NULL
    
    # filepath <- system.file("extdata", "metabolon_v3_example.xlsx", package = "metaboprep")
    # sheet = 'Batch-normalized Data'
    # feature_sheet = 'Chemical Annotation'
    # feature_id_col = 'CHEM_ID'
    # sample_sheet = 'Sample Meta Data'
    # sample_id_col = 'PARENT_SAMPLE_NAME'
  }

  
  # checks ====
  if(!grepl("(?i)\\.(xls|xlsx)$", filepath)){
    stop(paste0("Expected a commercial Metabolon excel sheet with extension .xls or .xlsx\n"), call.=FALSE)
  }

  
  # read data ====
  sheets <- readxl::excel_sheets(filepath)
  if (is.null(sheet)) {
    choice <- readline(prompt = paste0(c("Which sheet is the data of interest on? Choice: ", paste0("Enter ", seq_along(sheets), " for `", sheets, "`")), collapse="\n"))
    choice <- as.integer(choice)
  } else {
    choice <- sheet
  }
  
  
  # determine if version 3 (multi-tab) or not
  if (!is.null(feature_sheet) || !is.null(sample_sheet)) {
    
    # check 
    stopifnot("Separate sample and feature sheets indicated but not found in excel tabs" = all(c(feature_sheet, sample_sheet) %in% sheets))

        
    # the indicated data sheet
    dat <- suppressMessages(
      readxl::read_xlsx(filepath, sheet=choice, col_names=TRUE, na=c("","NA","NDEF","TAG")) |> as.data.frame()
    )
    data <- as.matrix(dat[,-1])
    rownames(data) <- dat[[1]]
    
    
    # features
    features <- suppressMessages(
      readxl::read_xlsx(filepath, sheet=feature_sheet, col_names=TRUE, na=c("","NA","NDEF","TAG")) |> as.data.frame()
    )
    

    # samples
    samples <- suppressMessages(
      readxl::read_xlsx(filepath, sheet=sample_sheet, col_names=TRUE, na=c("","NA","NDEF","TAG")) |> as.data.frame()
    )
    
    
    # rename
    stopifnot("feature_id_col provided but not found in feature sheet" = feature_id_col %in% names(features))
    names(features)[names(features) == feature_id_col] <- "feature_id"
    stopifnot("sample_id_col provided but not found in sample sheet" = sample_id_col %in% names(samples))
    names(samples)[names(samples) == sample_id_col] <- "sample_id"
    
    
  } else {
  
    # try extracting as if version 1 or 2 format
    raw <- suppressMessages(
      readxl::read_xlsx(filepath, sheet=choice, col_names=FALSE, na=c("","NA","NDEF","TAG")) |> as.data.frame()
    )
    
    # get data structure & rename ====
    data_header_row  <- which(!is.na(raw[, 1]))[1]
    batch_header_col <- which(!is.na(raw[1, ]))[1]
    
    # features
    features <- raw[(data_header_row+1L):nrow(raw), 1L:batch_header_col]
    cnames   <- unlist(raw[data_header_row, 1L:batch_header_col])
    names(features) <- clean_names(cnames)
    
    if (any(grepl("(?i)COMP.*?ID", names(features)))) { 
      
      names(features)[grep("(?i)COMP.*?ID", names(features))[1]] <- "feature_id"
      
    } else if (any(grepl("(?i)metabolite.*?id", names(features)))) {
      
      names(features)[grep("(?i)metabolite.*?id", names(features))[1]] <- "feature_id"
      
    } else {
      
      warning("No feature ID column found, defaulting to sequential IDs")
      features[["feature_id"]] <- paste0("FID_", seq_len(nrow(features)))
      
    }
    features <- features[, c("feature_id", setdiff(names(features), "feature_id"))]
    
    
    # samples
    samples <- t(raw[1L:(data_header_row-1L), (batch_header_col+1L):ncol(raw)])
    samples <- as.data.frame(samples)
    cnames  <- raw[1L:(data_header_row-1L), ][[batch_header_col]]
    names(samples) <- clean_names(cnames)
    
    if (any(grepl("(?i)sample.*?name", names(samples)))) { 
      
      names(samples)[which(grepl("(?i)sample.*?name", names(samples)))[1]] <- "sample_id"
      
    } else {
      
      warning("No sample ID column found, defaulting to sequential IDs")
      samples[["sample_id"]] <- paste0("SID_", seq_len(nrow(samples)))
      
    }
    samples <- samples[, c("sample_id", setdiff(names(samples), "sample_id"))]
    
    
    # data
    raw_subset     <- raw[(data_header_row + 1L):nrow(raw), (batch_header_col + 1L):ncol(raw)]
    data           <- as.data.frame(lapply(raw_subset, function(x) as.numeric(gsub(",", "", x))))
    data           <- t(as.matrix(data))
    colnames(data) <- features$feature_id
    rownames(data) <- samples$sample_id
  
  }

  
  # checks
  stopifnot("Sample ids do not exactly match row names of data" = identical(as.character(samples$sample_id), as.character(rownames(data))))
  stopifnot("Feature ids do not exactly match row names of data" = identical(as.character(features$feature_id), as.character(colnames(data))))
  
  
  # return ====
  return(list(data       = data,
              samples    = samples,
              features   = features))
  
}


