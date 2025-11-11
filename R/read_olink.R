#' @title Read and Process Olink NPX Data File
#' @description
#'  This function reads and processes an Olink NPX file in long format. It supports `.csv`, `.xls`, `.xlsx`, `.txt`, `.zip`, and `.parquet` formats, using Olink's own OlinkAnalyze::read_NPX() function, and returns a metaboprep object or a list of matrices and metadata frames for further analysis.
#'
#' @param filepath A string specifying the path to the Olink NPX file.
#' @param return_Metaboprep logical, if TRUE (default) return a Metaboprep object, if FALSE return a list.
#' 
#' @returns Metaboprep object or a named list with the following elements:
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
#'   filepath <- system.file("extdata", "example_olink_data.txt", package = "metaboprep")
#'   olink_data <- read_olink(filepath)
#' }
#' @importFrom OlinkAnalyze read_NPX
#' @importFrom reshape2 dcast
#' @export

read_olink <- function(filepath, return_Metaboprep = FALSE) {
  
  # testing ====
  if (FALSE) {
    filepath <- system.file("extdata", "olink_v1_example.txt", package = "metaboprep")
  }
  
  
  # checks 1 ====
  if (!grepl("(?i)\\.(csv|xls|xlsx|zip|txt|parquet)$", filepath)) {
    stop(paste0("Expected a commercial Olink file with extension .csv, .xls, .xlsx, .txt, .zip, or .parquet"), call. = FALSE)
  }
  
  
  # read data ====
  df <- OlinkAnalyze::read_NPX(filepath)
  if (!"SampleID" %in% colnames(df)) {
    stop("Column 'SampleID' not found in the dataset. Either your data is not Olink or you have renamed your sample column for some reason. Suggest you use an alternative approach to reading in your data (see Vignette XXX).", call. = FALSE)
  }
  
  
  # checks 2 ====
  if (!any(duplicated(df$SampleID))) {
    stop("The dataset does not appear to be in long format - no duplicate SampleID values found. Suggest you use an alternative approach to reading in your data (see Vignette XXX).", call. = FALSE)
  }
  
  
  # init ====
  data <- NULL
  controls <- NULL
  features <- NULL
  samples <- NULL
  control_metadata <- NULL
  
  
  # feature data for SAMPLES ====
  ## olink have multiple versions, some have a Sample_Type column (SAMPLE, CONTROL) some don't and instead have "CONTROL" in SampleID column
  if ("Sample_Type" %in% colnames(df)) {
    
    df_features_samples <- df[df$Sample_Type == "SAMPLE" & !grepl("empty well", df$SampleID, ignore.case = TRUE), ]
    
  } else if (any(grepl("CONTROL", df$SampleID, ignore.case = TRUE))) {
    
    df_features_samples <- df[!grepl("CONTROL", df$SampleID, ignore.case = TRUE) & !grepl("empty well", df$SampleID, ignore.case = TRUE), ]
    
  } else {
    
    stop("The dataset does not contain a 'Sample_Type' column or any 'CONTROL' entries in 'SampleID'. It is likely not an Olink file. Suggest you use an alternative approach to reading in your data (see Vignette XXX)", call. = FALSE)
  
  }

  data           <- reshape2::dcast(df_features_samples, SampleID ~ OlinkID, value.var = "NPX")
  rownames(data) <- data$SampleID
  data           <- as.matrix(data[ , setdiff(colnames(data), "SampleID") ])
    
  
  # feature data for CONTROLS ====
  ## olink have multiple versions, some have a Sample_Type column (SAMPLE, CONTROL) some don't and instead have "CONTROL" in SampleID column
  if ("Sample_Type" %in% colnames(df)) {
    
    df_features_controls <- df[df$Sample_Type == "CONTROL" | grepl("empty well", df$SampleID, ignore.case = TRUE), ]
    
  } else if (any(grepl("CONTROL", df$SampleID, ignore.case = TRUE))) {
    
    df_features_controls <- df[grepl("CONTROL", df$SampleID, ignore.case = TRUE) | grepl("empty well", df$SampleID, ignore.case = TRUE), ]
  
  } else {
    
    stop("The dataset does not contain a 'Sample_Type' column or any 'CONTROL' entries in 'SampleID'. It is likely not an Olink file. Suggest you use an alternative approach to reading in your data (see Vignette XXX)", call. = FALSE)
  
  }
  
  controls           <- reshape2::dcast(df_features_controls, SampleID ~ OlinkID, value.var = "NPX")
  rownames(controls) <- controls$SampleID
  controls_matrix    <- as.matrix(controls[ , setdiff(colnames(controls), "SampleID") ])
  
  
  # feature meta-data ====
  features <- df_features_samples[
    , setdiff(colnames(df_features_samples), c("SampleID", "NPX", "QC_Warning", "Index", "PlateID")), 
    drop = FALSE
  ]
  features <- unique(features)
  names(features)[names(features) == "OlinkID"] <- "feature_id"
  
  control_feature_meta <- df_features_controls[
    , setdiff(colnames(df_features_controls), c("SampleID", "NPX", "QC_Warning", "Index", "PlateID")), 
    drop = FALSE
  ]
  control_feature_meta <- unique(control_feature_meta)
  names(control_feature_meta)[names(control_feature_meta) == "OlinkID"] <- "feature_id"
  
  
  # sample meta-data ====
  samples <- df_features_samples[
    , setdiff(colnames(df_features_samples), c("Index", "OlinkID", "UniProt", "Assay", "Assay_Warning", "MissingFreq", "LOD", "NPX", "Panel", "Panel_Version", "Normalization")),
    drop = FALSE
  ]
  samples <- unique(samples)
  names(samples)[names(samples) == "SampleID"] <- "sample_id"
  
  control_sample_meta <- df_features_controls[
    , setdiff(colnames(df_features_controls), c("Index", "OlinkID", "UniProt", "Assay", "Assay_Warning", "MissingFreq", "LOD", "NPX", "Panel", "Panel_Version", "Normalization")),
    drop = FALSE
  ]
  control_sample_meta <- unique(control_sample_meta)
  names(control_sample_meta)[names(control_sample_meta) == "SampleID"] <- "sample_id"
  
  
  # return ====
  if (return_Metaboprep) {
    return(Metaboprep(data = data, 
                      samples = samples, 
                      features = features))
  } else {
    return(list(data = data, 
                samples = samples, 
                features = features, 
                controls = controls,
                control_metadata = control_sample_meta))
  }
  
}
