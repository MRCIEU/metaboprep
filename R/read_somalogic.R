#' @title Read and Process SomaLogic adat file
#' @description
#' This function reads and processes a commercial SomaLogic `.adat` file. It extracts
#' RFU (Relative Florecent Units) data for samples and controls, along with
#' their respective metadata and feature (protein) metadata. The function returns
#' a structured list suitable for further analysis.
#'
#' @param filepath A string specifying the path to the SomaLogic `.adat` file.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{data}{A matrix of RFU values for experimental samples, with `SampleId`
#'         as row names and `SeqId` (from columns containing "seq") as column names.}
#'   \item{samples}{A `tibble` containing metadata for experimental samples, with
#'         `sample_id` (renamed from `SampleId`) as the first column.}
#'   \item{features}{A `tibble` containing feature-level metadata (e.g., protein
#'         details), including a newly created `feature_id` column derived from `SeqId`.}
#'   \item{controls}{A matrix of RFU values for control samples (specifically "Calibrator"
#'         samples), with `SampleId` as row names and `SeqId` as column names.}
#'   \item{control_metadata}{A `tibble` containing metadata for control samples
#'         (specifically "Calibrator" samples), with `sample_id` as the first column.}
#' }
#'
#' @details
#' The function performs several validation steps and data transformations:
#' \itemize{
#'   \item It first checks if the provided `filepath` points to an `.adat` file.
#'   \item It uses `SomaDataIO::read_adat()` to import the raw data and
#'         `SomaDataIO::is.soma_adat()` to verify its integrity as a SomaLogic object.
#'   \item It confirms the presence of the crucial `SampleId` column.
#'   \item Data is separated into experimental "Sample" and "Calibrator" control groups based on the `SampleType` column.
#'   \item For both sample and control data, RFU values corresponding to "seq" columns
#'         are extracted and reshaped into wide matrices with `SampleId` as row names.
#'   \item Feature metadata is extracted from `attr(df, "Col.Meta")`, and a new
#'         `feature_id` column is created (prefixed with "seq." and hyphens replaced by periods).
#'   \item Sample and control metadata are extracted from `attr(df, "row_meta")`,
#'         converted to `tibble`s, renamed, and `sample_id` is relocated to the front.
#'         Explicit `tibble::as_tibble()` and `tibble::remove_rownames()` are used
#'         to handle potential `SomaDataIO` object intricacies.
#' }
#'
#' @examples
#' \dontrun{
#'   filepath <- system.file("extdata", "example_data10.adat", package = "SomaDataIO")
#'   somalogic_data <- read_somalogic(filepath)
#' }
#' @importFrom SomaDataIO read_adat is.soma_adat
#' @export
read_somalogic <- function(filepath) {
  
  # testing ====
  if (FALSE) {
    filepath <- system.file("extdata", "example_data10.adat",
                             package = "SomaDataIO", mustWork = TRUE)
  }
  
  
  # checks 1 ====
  if (!grepl("(?i)\\.(adat)$", filepath)) {
    stop(paste0("Expected a commercial Somalogic file with extension .adat"), call. = FALSE)
  }
  
  
  # read data ====
  df <- SomaDataIO::read_adat(file = filepath)

  
  # checks 2 ====
  if (!SomaDataIO::is.soma_adat(df)) {
    stop("The file is not a SomaLogic 'adat' file.")
  }  
  if (!"SampleId" %in% colnames(df)) {
    stop("Column 'SampleId' not found in the dataset. Either your data is not Somalogic or you have renamed your sample column for some reason. Suggest you use an alternative approach to reading in your data (see Vignette XXX).", call. = FALSE)
  }
  
  
  # init ====
  data <- NULL
  controls <- NULL
  features <- NULL
  samples <- NULL
  control_metadata <- NULL
  
  
  # feature data for SAMPLES ====
  data           <- df[df$SampleType == "Sample", ]
  rownames(data) <- data$SampleId
  data           <- data[, grep("seq", colnames(data), value = TRUE)]
  data           <- as.matrix(data)

  
  # feature data for CONTROLS ====
  controls           <- df[df$SampleType == "Calibrator", ]
  rownames(controls) <- controls$SampleId
  controls           <- controls[, grep("seq", colnames(controls), value = TRUE)]
  controls           <- as.matrix(controls)
    
  
  # feature meta-data ====
  features            <- attr(df, "Col.Meta") 
  features$feature_id <- paste0("seq.", gsub("-", ".", features$SeqId))
  features            <- features[, c("feature_id", setdiff(names(features), "feature_id"))]

  
  # sample meta-data ====
  row_meta <- attr(df, "row_meta")
  plain_df <- as.data.frame(df)
  samples  <- plain_df[, row_meta, drop = FALSE]
  names(samples)[names(samples) == "SampleId"] <- "sample_id"
  samples  <- samples[c("sample_id", setdiff(names(samples), "sample_id"))]
  samples  <- samples[samples$SampleType == "Sample", ]
  
  control_sample_meta <- plain_df[, row_meta, drop = FALSE]
  names(control_sample_meta)[names(control_sample_meta) == "SampleId"] <- "sample_id"
  control_sample_meta <- control_sample_meta[c("sample_id", setdiff(names(control_sample_meta), "sample_id"))]
  control_sample_meta <- control_sample_meta[control_sample_meta$SampleType == "Calibrator", ]
  
  
  # return ====
  return(list(data             = data,
              samples          = samples,
              features         = features,
              controls         = controls,
              control_metadata = control_sample_meta))
  
}