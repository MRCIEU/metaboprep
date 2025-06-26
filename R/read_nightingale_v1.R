#' @title Read Nightingale Data (format 1)
#' @param filepath character, commercial Nightingale excel sheet with extension .xls or .xlsx
#' @returns list,  list(data = 3D matrix, samples = samples data.table, features = features data.table)
#'
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom utils zip
#'
#' @export
#'
read_nightingale_v1 <- function(filepath) {
  # 
  # # testing
  # if (FALSE) {
  #   filepath <- system.file("extdata", "nightingale_v1_example.xlsx", package = "metaboprep2")
  # }
  # 
  # # check excel file
  # if(!grepl("(?i)\\.(xls|xlsx)$", filepath)){
  #   stop(paste0("Expected a commercial Nightingale excel sheet with extension .xls or .xlsx\n"), call.=FALSE)
  # }
  # 
  # # check sheet names
  # sheets <- readxl::excel_sheets(filepath)
  # expected_sheets <- c("Results", "Quality control tags and notes", "Tags per biomarker", "Biomarker annotations", "Tag annotations")
  # if (!all(expected_sheets %in% sheets)) {
  #   stop(
  #     paste0("excel sheets not found:\n",
  #            " - expected: ", paste("`", expected_sheets, "`", sep="", collapse=", "), "\n",
  #            " - observed: ", paste("`", sheets, "`", sep="", collapse=", ")
  #     )
  #   )
  # }
  # 
  # # rename sheets
  # sheets    <- sheets[sheets %in% expected_sheets]
  # std_names <- list("Results"                        = "raw",
  #                   "Quality control tags and notes" = "sample_tags",
  #                   "Tags per biomarker"             = "feature_tags",
  #                   "Biomarker annotations"          = "feature_annotations",
  #                   "Tag annotations"                = "sample_annotations")
  # names(sheets) <- std_names[sheets]
  # 
  # # get the feature annotations data
  # features <- suppressMessages(
  #   readxl::read_xlsx(filepath, sheet=sheets[["feature_annotations"]], na=c("","NA","NDEF","TAG")) |> data.table::as.data.table()
  # )[, list(feature_id      = `Excel column name`,
  #          feature_name    = `Biomarker name`,
  #          feature_unit    = `Unit`,
  #          pathway         = `Group`,
  #          sub_pathway     = `Subgroup`)]
  # features <- annotate_features(features,
  #                               fixed_match_cols = list(name = "feature_name"),
  #                               fuzzy_match_cols = list(name = NULL)) # dont fuzzy match
  # 
  # # must have columns
  # features[, `:=`(pathway  = anno_main_class,
  #                 platform = NA_character_,
  #                 derived_feature = anno_derived_feature)]
  # 
  # 
  # # get the sample annotations data
  # samp_annot <- suppressMessages(
  #   readxl::read_xlsx(filepath, sheet=sheets[["sample_tags"]], na=c("","NA","NDEF","TAG")) |> data.table::as.data.table()
  # )
  # 
  # # get sample data positioning
  # top_corner <- as.matrix(samp_annot[1:ifelse(nrow(samp_annot)<20,nrow(samp_annot),20), 1:ifelse(nrow(samp_annot)<10,nrow(samp_annot),10)])
  # head_inds  <- which(top_corner == "Sample id", arr.ind = TRUE) + c(-1,1)
  # data_inds  <- which(top_corner == "Sample id", arr.ind = TRUE) + c(3,0)
  # 
  # # extract sample data
  # samples <- samp_annot[data_inds[1L,"row"]:nrow(samp_annot), data_inds[1L,"col"]:ncol(samp_annot)] |> data.table::as.data.table()
  # names(samples) <- c("sample_id", unlist(samp_annot[head_inds[1L,"row"]:head_inds[1L,"row"], head_inds[1L,"col"]:ncol(samp_annot)]))
  # 
  # # get raw data
  # raw <- suppressMessages(
  #   readxl::read_xlsx(filepath, sheet=sheets[["raw"]], col_names=FALSE, na=c("","NA","NDEF","TAG")) |> data.table::as.data.table()
  # )
  # 
  # # get data positioning
  # top_corner <- as.matrix(raw[1:ifelse(nrow(raw)<20,nrow(raw),20), 1:ifelse(nrow(raw)<10,nrow(raw),10)])
  # head_inds  <- which(top_corner == "Sample id", arr.ind = TRUE)
  # data_inds  <- which(top_corner == "Subgroup", arr.ind = TRUE) + 1
  # 
  # # get the samples
  # raw_sample_id_order <- unname(unlist(raw[data_inds[1L,"row"]:nrow(raw), head_inds[1L,"col"]:head_inds[1L,"col"]]))
  # samples <- samples[order(match(sample_id, raw_sample_id_order))]
  # stopifnot(identical(samples$sample_id, raw_sample_id_order))
  # 
  # # get the features
  # raw_feature_id_order <- unname(unlist(raw[head_inds[1L,"row"]:head_inds[1L,"row"], (head_inds[1L,"col"]+1):ncol(raw)]))
  # features <- features[order(match(feature_id, raw_feature_id_order))]
  # stopifnot(identical(features$feature_id, raw_feature_id_order))
  # 
  # # get the data
  # data <- as.matrix(raw[data_inds[1L,"row"]:nrow(raw), data_inds[1L,"col"]:ncol(raw)][, lapply(.SD, function(x) as.numeric(gsub(",","",x)))])
  # data <- array(data,
  #               dim = c(nrow(data), ncol(data), 1),
  #               dimnames = list(samples$sample_id, features$feature_id, "raw"))
  # 
  # # return
  # return(list(data       = data,
  #             samples    = samples,
  #             features   = features))
}
