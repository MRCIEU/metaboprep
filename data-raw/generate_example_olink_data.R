# data-raw/generate_example_olink_data.R

rm(list=ls())
set.seed(821)

# Create 100 sample IDs: include some controls and empty wells
sample_ids <- c(
  paste0("Sample_", sprintf("%05d", 1:90)),
  paste0("CONTROL_", sprintf("%04d", 1:5)),
  paste0("Empty well_", sprintf("%02d", 1:5))
)

# Create 100 feature IDs
feature_ids <- paste0("OID", sprintf("%05d", 1:100))
uniprot_ids <- paste0("P", sample(10000:99999, 100))
assays <- paste0("Protein_", 1:100)

# Expand grid: all combinations of SampleID × FeatureID
example_olink_data <- expand.grid(
  SampleID = sample_ids,
  OlinkID = feature_ids,
  stringsAsFactors = FALSE
) %>%
  tibble::as_tibble() %>%
  # Add feature metadata columns by joining on OlinkID
  dplyr::left_join(
    tibble::tibble(
      OlinkID = feature_ids,
      UniProt = uniprot_ids,
      Assay = assays,
      MissingFreq = rep("1.47E-3", 100),
      Panel = rep("Olink Immuno-Oncology", 100),
      Panel_Version = rep("v.3112", 100),
      PlateID = rep("20211440-001_SP211650_IMO", 100),
      LOD = rep(2.14, 100)
    ),
    by = "OlinkID"
  ) %>%
  # Add row-wise unique Index and random NPX values
  dplyr::mutate(
    Index = row_number(),
    QC_Warning = sample(c("Pass", "Warning"), n(), replace = TRUE),
    NPX = round(runif(n(), 5, 10), 2),
    Normalization = "Intensity Normalized (v.2)"
  ) %>%
  # Reorder columns to match your structure
  dplyr::select(
    SampleID, Index, OlinkID, UniProt, Assay, MissingFreq, Panel,
    Panel_Version, PlateID, QC_Warning, LOD, NPX, Normalization
  )

# Create directory if needed
if (!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)

# Save as tab-delimited text file
write.table(
  example_olink_data,
  file = "inst/extdata/example_olink_data.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

message("Example Olink data saved to inst/extdata/example_olink_data.txt")