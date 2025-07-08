#' @title Read Metabolon Data
#' @param filepath character, commercial Metabolon excel sheet with extension .xls or .xlsx
#' @param sheet character or integer, the excel sheet name (or index) from which to read. 
#' @returns list,  list(data = matrix, samples = samples data.frame, features = features data.frame)
#'
#' @examples
#' # version 1 data format
#' filepath1 <- system.file("extdata", "metabolon_v1_example.xlsx", package = "metaboprep")
#' read_metabolon(filepath1, sheet = 2)
#' 
#' # version 2 data format
#' filepath2 <- system.file("extdata", "metabolon_v2_example.xlsx", package = "metaboprep")
#' read_metabolon(filepath2, sheet = 'OrigScale')
#'
#' @importFrom readxl excel_sheets read_xlsx
#' @export
read_metabolon <- function(filepath, sheet = NULL) {

  # testing ====
  if (FALSE) {
    filepath <- system.file("extdata", "metabolon_v1_example.xlsx", package = "metaboprep")
    #filepath <- system.file("extdata", "metabolon_v2_example.xlsx", package = "metaboprep")
  }

  
  # checks ====
  if(!grepl("(?i)\\.(xls|xlsx)$", filepath)){
    stop(paste0("Expected a commercial Metabolon excel sheet with extension .xls or .xlsx\n"), call.=FALSE)
  }

  
  # read data ====
  sheets <- readxl::excel_sheets(filepath)
  if (is.null(sheet)) {
    choice <- readline(prompt = paste0(c("Which sheet should be read? Choice: ", paste0("Enter ", seq_along(sheets), " for `", sheets, "`")), collapse="\n"))
    choice <- as.integer(choice)
  } else {
    choice <- sheet
  }
  
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
  

  
  # return ====
  return(list(data       = data,
              samples    = samples,
              features   = features))
  
}


