#' @title Read and Process Olink NPX Data File
#' @description
#'  This function reads and processes an Olink NPX file in long format. It supports `.csv`, `.xls`, `.xlsx`, `.txt`, `.zip`, and `.parquet` formats, and returns a list of matrices and metadata frames for further analysis.
#'
#' @param filepath A string specifying the path to the Olink NPX file.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{data}{A matrix of NPX values with `SampleID` as rows and `OlinkID` as columns, containing only sample data.}
#'   \item{samples}{A `data.frame` containing metadata for samples.}
#'   \item{features}{A `data.frame` containing feature-level metadata for samples.}
#'   \item{controls}{A matrix of NPX values for control samples.}
#'   \item{control_metadata}{A `data.frame` containing metadata for control samples.}
#' }
#'
#' @details
#' The function checks whether the input data is in long format by verifying the presence of duplicate `SampleID` values. It also accommodates two variants of Olink files:
#' \itemize{
#'   \item Files that include a `Sample_Type` column with values `"SAMPLE"` and `"CONTROL"`.
#'   \item Files that use the `SampleID` column to label control samples (e.g., entries containing `"CONTROL"`).
#' }
#'
#' If neither format is detected, the function stops with an error indicating that the data is likely not from Olink.
#'
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "example_olink_data.txt", package = "metaboprep")
#' olink_data <- read_olink_v1(filepath)
#' }
#'
#' @importFrom dplyr filter select rename
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom OlinkAnalyze read_NPX
#' @importFrom magrittr %>%
#' @export

read_olink_v1 <- function(filepath) {
  
  if (FALSE) {
    filepath <- system.file("extdata", "metabolon_v1_example.xlsx", package = "metaboprep")
  }
  
  # 
  if (!grepl("(?i)\\.(csv|xls|xlsx|zip|txt|parquet)$", filepath)) {
    stop(paste0("Expected a commercial Olink file with extension .csv, .xls, .xlsx, .txt, .zip, or .parquet"), call. = FALSE)
  }
  
  # read data ====
  df <- OlinkAnalyze::read_NPX(filepath)
  if (!"SampleID" %in% colnames(df)) {
    stop("Column 'SampleID' not found in the dataset. Either your data is not Olink or you have renamed your sample column for some reason. Suggest you use an alternative approach to reading in your data (see Vignette XXX).", call. = FALSE)
  }
  if (!any(duplicated(df$SampleID))) {
    stop("The dataset does not appear to be in long format - no duplicate SampleID values found. Suggest you use an alternative approach to reading in your data (see Vignette XXX).", call. = FALSE)
  }
  
  data <- NULL
  controls <- NULL
  features <- NULL
  samples <- NULL
  control_metadata <- NULL
  
  # feature data for SAMPLES ====
  ## olink have multiple versions, some have a Sample_Type column (SAMPLE, CONTROL) some don't and instead have "CONTROL" in SampleID column
  if ("Sample_Type" %in% colnames(df)) {
    df_features_samples <- df %>%
      filter(Sample_Type == "SAMPLE") %>%
      filter(!grepl("empty well", SampleID, ignore.case = TRUE))
  } else if (any(grepl("CONTROL", df$SampleID, ignore.case = TRUE))) {
    df_features_samples <- df %>%
      filter(!grepl("CONTROL", SampleID, ignore.case = TRUE)) %>%
      filter(!grepl("empty well", SampleID, ignore.case = TRUE))
  } else {
    stop("The dataset does not contain a 'Sample_Type' column or any 'CONTROL' entries in 'SampleID'. It is likely not an Olink file. Suggest you use an alternative approach to reading in your data (see Vignette XXX)", call. = FALSE)
  }

  data <- df_features_samples %>%
    dplyr::select(SampleID, OlinkID, NPX) %>%
    tidyr::pivot_wider(
      names_from = OlinkID,
      values_from = NPX
    ) %>%
    tibble::column_to_rownames(var = "SampleID") %>%  # from tibble::column_to_rownames
    as.matrix()
    
  # feature data for CONTROLS ====
  ## olink have multiple versions, some have a Sample_Type column (SAMPLE, CONTROL) some don't and instead have "CONTROL" in SampleID column
  if ("Sample_Type" %in% colnames(df)) {
    df_features_controls <- df %>% 
      filter(Sample_Type == "CONTROL" | grepl("empty well", SampleID, ignore.case = TRUE))
  } else if (any(grepl("CONTROL", df$SampleID, ignore.case = TRUE))) {
    df_features_controls <- df %>% 
      filter(grepl("CONTROL", SampleID, ignore.case = TRUE) | 
               grepl("empty well", SampleID, ignore.case = TRUE))
  } else {
    stop("The dataset does not contain a 'Sample_Type' column or any 'CONTROL' entries in 'SampleID'. It is likely not an Olink file. Suggest you use an alternative approach to reading in your data (see Vignette XXX)", call. = FALSE)
  }
  
  controls <- df_features_controls %>%
    dplyr::select(SampleID, OlinkID, NPX) %>%
    tidyr::pivot_wider(
      names_from = OlinkID,
      values_from = NPX
    ) %>%
    tibble::column_to_rownames(var = "SampleID") %>%  # from tibble::column_to_rownames
    as.matrix()
  
  # feature meta-data ====
  features <- df_features_samples %>%
    dplyr::select(
      setdiff(colnames(.), c("SampleID", "NPX", "QC_Warning", "Index", "PlateID"))
    ) %>%
    unique() %>%
    dplyr::rename(feature_id = OlinkID)
  
  control_feature_meta <- df_features_controls %>%
    dplyr::select(
      setdiff(colnames(.), c("SampleID", "NPX", "QC_Warning", "Index", "PlateID"))
    ) %>%
    unique() %>%
    dplyr::rename(feature_id = OlinkID)
  
  # sample meta-data ====
  samples <- df_features_samples %>%
    dplyr::select(
      setdiff(colnames(.), c("Index", "OlinkID", "UniProt", "Assay", "Assay_Warning", "MissingFreq", "LOD", "NPX", "Panel", "Panel_Version", "Normalization"))
    ) %>%
    unique() %>%
    dplyr::rename(sample_id = SampleID)
  
  control_sample_meta <- df_features_controls %>%
    dplyr::select(
      setdiff(colnames(.), c("Index", "OlinkID", "UniProt", "Assay", "Assay_Warning", "MissingFreq", "LOD", "NPX", "Panel", "Panel_Version", "Normalization"))
    ) %>%
    unique() %>%
    dplyr::rename(sample_id = SampleID)
  
  
  # return ====
  return(list(data       = data,
              samples    = samples,
              features   = features,
              controls = controls,
              control_metadata = control_sample_meta))
}
