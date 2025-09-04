#' @title Read Nightingale Data (format 1)
#' @param filepath character, commercial Nightingale excel sheet with extension .xls or .xlsx
#' @param return_Metaboprep logical, if TRUE (default) return a Metaboprep object, if FALSE return a list.
#' @returns list or Metaboprep object, list(data = matrix, samples = samples data.frame, features = features data.frame)
#'
#' @examples
#' # version 1 data format
#' filepath1 <- system.file("extdata", "nightingale_v1_example.xlsx", package = "metaboprep")
#' m <- read_nightingale(filepath1)
#' 
#' # version 2 data format
#' filepath2 <- system.file("extdata", "nightingale_v2_example.xlsx", package = "metaboprep")
#' m <- read_nightingale(filepath2)
#'
#' @importFrom readxl excel_sheets read_xlsx
#' @export
read_nightingale <- function(filepath, return_Metaboprep = TRUE) {

  # testing ====
  if (FALSE) {
    filepath <- system.file("extdata", "nightingale_v1_example.xlsx", package = "metaboprep")
    filepath <- system.file("extdata", "nightingale_v2_example.xlsx", package = "metaboprep")
  }

  
  # checks ====
  if(!grepl("(?i)\\.(xls|xlsx)$", filepath)){
    stop(paste0("Expected a commercial Nightingale excel sheet with extension .xls or .xlsx\n"), call.=FALSE)
  }

  
  # read data ====
  sheets <- readxl::excel_sheets(filepath)

  # single work sheet format ====
  if (all(sheets %in% c("Worksheet")) || length(sheets)==1) {
    
    raw <- suppressMessages(
      readxl::read_xlsx(filepath, sheet=sheets, col_names=FALSE, na=c("","NA","NDEF","TAG")) |> as.data.frame()
    )

    # data positioning
    top_corner <- as.matrix(raw[1:ifelse(nrow(raw)<20,nrow(raw),20), 1:ifelse(nrow(raw)<10,nrow(raw),10)])
    head_inds  <- which(top_corner == "sampleid", arr.ind = TRUE)
    data_inds  <- which(top_corner == "success %", arr.ind = TRUE) + c(1,1)

    # samples
    raw_sample_ids    <- unname(unlist(raw[data_inds[1L, "row"]:nrow(raw), head_inds[1L, "col"]:head_inds[1L, "col"]]))
    samples           <- raw[data_inds[1L, "row"]:nrow(raw), 1:(head_inds[1L, "col"] -1 )]
    samples[]         <- lapply(samples, function(x) as.integer(!is.na(x)))
    names(samples)    <- unlist(raw[(head_inds[1L, "row"] - 1), 1:(head_inds[1L, "col"] - 1)])
    samples$sample_id <- raw_sample_ids
    names(samples)    <- clean_names(names(samples))
    samples           <- samples[, c("sample_id", setdiff(names(samples), "sample_id"))]

    # features
    features <- data.frame(
      "feature_id" = unname(unlist(raw[head_inds[1L, "row"], (head_inds[1L, "col"] + 1):ncol(raw)]))
    )

    # data
    data           <- raw[data_inds[1, "row"]:nrow(raw), data_inds[1, "col"]:ncol(raw)]
    data           <- apply(data, 2, function(x) as.numeric(gsub(",", "", x)))
    data           <- as.matrix(data)
    rownames(data) <- samples$sample_id
    colnames(data) <- features$feature_id
    
    
  # multi work sheet format ====
  } else if (all(sheets %in% c("Results", "Quality control tags and notes", "Tags per biomarker", "Biomarker annotations", "Tag annotations"))) {
    
    # features
    features <- suppressMessages(
      readxl::read_xlsx(filepath, sheet="Biomarker annotations", na=c("","NA","NDEF","TAG")) |> as.data.frame()
    )
    names(features)[names(features) == "Excel column name"] <- "feature_id"
    
    # samples
    samp_annot <- suppressMessages(
      readxl::read_xlsx(filepath, sheet="Quality control tags and notes", na=c("","NA","NDEF","TAG")) |> as.data.frame()
    )
    
    # sample data positioning
    top_corner <- as.matrix(samp_annot[1:ifelse(nrow(samp_annot)<20,nrow(samp_annot),20), 1:ifelse(nrow(samp_annot)<10,nrow(samp_annot),10)])
    head_inds  <- which(top_corner == "Sample id", arr.ind = TRUE) + c(-1,1)
    data_inds  <- which(top_corner == "Sample id", arr.ind = TRUE) + c(3,0)
    
    # sample data
    samples <- samp_annot[data_inds[1L, "row"]:nrow(samp_annot), data_inds[1L, "col"]:ncol(samp_annot)]
    names(samples) <- c("sample_id", unlist(samp_annot[head_inds[1L,"row"]:head_inds[1L,"row"], head_inds[1L,"col"]:ncol(samp_annot)]))
    names(samples) <- clean_names(names(samples))
    
    # data
    raw <- suppressMessages(
      readxl::read_xlsx(filepath, sheet="Results", col_names=FALSE, na=c("","NA","NDEF","TAG")) |> as.data.frame()
    )
    
    # data positioning
    top_corner <- as.matrix(raw[1:ifelse(nrow(raw)<20,nrow(raw),20), 1:ifelse(nrow(raw)<10,nrow(raw),10)])
    head_inds  <- which(top_corner == "Sample id", arr.ind = TRUE)
    data_inds  <- which(top_corner == "Subgroup", arr.ind = TRUE) + 1
    
    # samples
    raw_sample_id_order <- unname(unlist(raw[data_inds[1L,"row"]:nrow(raw), head_inds[1L,"col"]:head_inds[1L,"col"]]))
    samples <- samples[order(match(samples$sample_id, raw_sample_id_order)), ]
    stopifnot(identical(as.character(samples$sample_id), as.character(raw_sample_id_order)))
    
    # features
    raw_feature_id_order <- unname(unlist(raw[head_inds[1L,"row"]:head_inds[1L,"row"], (head_inds[1L,"col"]+1):ncol(raw)]))
    features             <- features[order(match(features$feature_id, raw_feature_id_order)), ]
    names(features)      <- clean_names(names(features))
    stopifnot(identical(features$feature_id, raw_feature_id_order))
    
    # data
    data           <- raw[data_inds[1L,"row"]:nrow(raw), data_inds[1L,"col"]:ncol(raw)]
    data           <- as.data.frame(lapply(data, function(x) as.numeric(gsub(",", "", x))))
    data           <- as.matrix(data)
    rownames(data) <- samples$sample_id
    colnames(data) <- features$feature_id
    
  } else {
    
    stop("Excel sheet names do not match known formats")
    
  }
  
  
  # return ====
  if (return_Metaboprep) {
    return(Metaboprep(data = data, samples = samples, features = features))
  } else {
    return(list(data = data, samples = samples, features = features))
  }
  
}
