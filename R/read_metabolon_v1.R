#' @title Read Metabolon Data (format 1)
#' @param filepath character, commercial Metabolon excel sheet with extension .xls or .xlsx
#' @returns list,  list(data = 3D matrix, samples = samples data.frame, features = features data.frame)
#'
#' @examples
#' filepath <- system.file("extdata", "metabolon_v1_example.xlsx", package = "metaboprep2")
#' read_metabolon_v1(filepath)
#'
#' @importFrom readxl excel_sheets read_xlsx
#' @export
read_metabolon_v1 <- function(filepath) {

  # testing
  if (FALSE) {
    filepath <- system.file("extdata", "metabolon_v1_example.xlsx", package = "metaboprep")
  }

  if(!grepl("(?i)\\.(xls|xlsx)$", filepath)){
    stop(paste0("Expected a commercial Metabolon excel sheet with extension .xls or .xlsx\n"), call.=FALSE)
  }

  sheets <- readxl::excel_sheets(filepath)
  sheets <- sheets[sheets %in% c("OrigScale", "ScaledImp")]
  std_names <- list("OrigScale" = "raw",
                    "ScaledImp" = "scaled")
  names(sheets) <- std_names[sheets]

  data_list <- list()
  features <- NULL
  samples <- NULL
  comp_ids <- NULL

  for (i in seq_along(sheets)) {

    raw <- suppressMessages(
      readxl::read_xlsx(filepath, sheet=sheets[i], col_names=FALSE, na=c("","NA","NDEF","TAG"))
    )

    data_header_row  <- which(!is.na(raw[, 1]))[1]
    batch_header_col <- which(!is.na(raw[1, ]))[1]

    if (is.null(features)) {
      features <- raw[(data_header_row+1L):nrow(raw), 1L:batch_header_col]
      cnames   <- unlist(raw[data_header_row, 1L:batch_header_col])
      cnames[grep("(?i)hmdb", cnames)] <- "hmdb"
      names(features) <- clean_names(cnames)

      # must have columns
      comp_id_col  <- grep("(?i)COMP.*?ID", names(features), value = TRUE)[1]
      pathway_col  <- grep("(?i)pathway", names(features), value = TRUE)[1]
      platform_col <- grep("(?i)platform", names(features), value = TRUE)[1]
      names(features)[names(features) %in% c(comp_id_col, pathway_col, platform_col)] <- c("comp_id", "pathway", "platform")

      # must have columns
      features$feature_id      <- paste0("comp_id_", features$comp_id)

    }

    if (is.null(samples)) {
      samples <- t(raw[1L:(data_header_row-1L), (batch_header_col+1L):ncol(raw)])
      samples <- as.data.frame(samples)
      cnames  <- raw[1L:(data_header_row-1L), ][[batch_header_col]]
      names(samples) <- clean_names(cnames)
      names(samples)[which(grepl("(?i)sample.*?name", names(samples)))[1]] <- "sample_id"
    }

    raw_subset <- raw[(data_header_row + 1L):nrow(raw), (batch_header_col + 1L):ncol(raw)]
    data <- as.data.frame(lapply(raw_subset, function(x) as.numeric(gsub(",", "", x))))
    colnames(data) <- features$feature_id
    rownames(data) <- samples$sample_id

    data_list[[names(sheets)[i]]] <- data
  }

  data <- array(unlist(data_list, use.names = FALSE),
                dim = c(nrow(data_list[[1]]), ncol(data_list[[1]]), length(data_list)),
                dimnames = list(rownames(data_list[[1]]), colnames(data_list[[1]]), names(sheets)))

  return(list(data       = data,
              samples    = samples,
              features   = features))
}
