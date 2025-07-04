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
#' filepath <- system.file("extdata", "example_data10.adat", package = "SomaDataIO")
#' somalogic_data <- read_somalogic(filepath)
#'
#' @importFrom dplyr filter select rename mutate relocate
#' @importFrom tidyr contains
#' @importFrom tibble remove_rownames column_to_rownames as_tibble
#' @importFrom SomaDataIO read_adat is.soma_adat
#' @importFrom stringr str_replace_all
#' @importFrom magrittr %>%
#' @export
read_somalogic <- function(filepath) {
  
  if (FALSE) {
    filepath <- system.file("extdata", "example_data10.adat",
                             package = "SomaDataIO", mustWork = TRUE)
  }
  
  # 
  if (!grepl("(?i)\\.(adat)$", filepath)) {
    stop(paste0("Expected a commercial Somalogic file with extension .adat"), call. = FALSE)
  }
  
  # read data ====
  df <- SomaDataIO::read_adat(file = filepath)

  if (!SomaDataIO::is.soma_adat(df)) {
    stop("The file is not a SomaLogic 'adat' file.")
  }  
  if (!"SampleId" %in% colnames(df)) {
    stop("Column 'SampleId' not found in the dataset. Either your data is not Somalogic or you have renamed your sample column for some reason. Suggest you use an alternative approach to reading in your data (see Vignette XXX).", call. = FALSE)
  }
  
  data <- NULL
  controls <- NULL
  features <- NULL
  samples <- NULL
  control_metadata <- NULL
  
  # feature data for SAMPLES ====
  df_features_samples <- df %>%
    dplyr::filter(SampleType == "Sample")
  
  data <- df_features_samples %>%
    dplyr::select(SampleId, tidyr::contains("seq")) %>%
    tibble::remove_rownames() %>% # Add this step to ensure no existing row names
    tibble::column_to_rownames(var = "SampleId") %>%
    as.matrix()
  
  # feature data for CONTROLS ====
  df_features_controls <- df %>%
    dplyr::filter(SampleType == "Calibrator")
  
  controls <- df_features_controls %>%
    dplyr::select(SampleId, tidyr::contains("seq")) %>%
    tibble::remove_rownames() %>% 
    tibble::column_to_rownames(var = "SampleId") %>%
    as.matrix()
    
  # feature meta-data ====
  features <- attr(df, "Col.Meta") %>%
    dplyr::mutate(
      feature_id = paste0("seq.", stringr::str_replace_all(SeqId, "-", "."))
    ) %>%
    dplyr::relocate(feature_id, .before = 1)

  # sample meta-data ====
  samples <- df %>%
    tibble::as_tibble() %>% # Convert to a regular tibble first to strip special attributes
    dplyr::select(attr(df, "row_meta")) %>% # Now select columns from the plain tibble
    tibble::remove_rownames() %>%
    dplyr::rename(sample_id = SampleId) %>%
    dplyr::relocate(sample_id, .before = 1) %>%
    dplyr::filter(SampleType == "Sample")
  
  control_sample_meta <- df %>%
    tibble::as_tibble() %>% # Convert to a regular tibble first to strip special attributes
    dplyr::select(attr(df, "row_meta")) %>% # Now select columns from the plain tibble
    tibble::remove_rownames() %>%
    dplyr::rename(sample_id = SampleId) %>%
    dplyr::relocate(sample_id, .before = 1) %>%
    dplyr::filter(SampleType == "Calibrator")
  
  # return ====
  return(list(data       = data,
              samples    = samples,
              features   = features,
              controls = controls,
              control_metadata = control_sample_meta))
  
}