#' Example Olink NPX Data
#'
#' A simulated dataset mimicking the structure of Olink NPX data with 100 samples
#' (including controls and empty wells) and 100 features (proteins).
#'
#' This dataset includes columns such as SampleID, Index, OlinkID, UniProt ID, Assay name,
#' Missing Frequency, Panel information, PlateID, QC warnings, Limit of Detection (LOD),
#' Normalized Protein Expression (NPX), and Normalization method.
#'
#' @format A tibble with 10,000 rows and 13 columns:
#' \describe{
#'   \item{SampleID}{Character. Unique sample identifier including samples, controls, and empty wells.}
#'   \item{Index}{Integer. Row index.}
#'   \item{OlinkID}{Character. Protein feature identifier.}
#'   \item{UniProt}{Character. UniProt accession number for the protein.}
#'   \item{Assay}{Character. Protein assay name.}
#'   \item{MissingFreq}{Character. Frequency of missing values as a string.}
#'   \item{Panel}{Character. Olink panel name.}
#'   \item{Panel_Version}{Character. Version of the panel.}
#'   \item{PlateID}{Character. Plate identifier.}
#'   \item{QC_Warning}{Character. QC warning status ("Pass" or "Warning").}
#'   \item{LOD}{Numeric. Limit of Detection for the protein.}
#'   \item{NPX}{Numeric. Normalized Protein Expression value.}
#'   \item{Normalization}{Character. Normalization method used.}
#' }
#'
#' @source Simulated dataset generated using \code{data-raw/generate_example_olink_data.R} and saved as a tab-delimited text file.
"example_olink_data"
