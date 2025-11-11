# Read and Process SomaLogic adat file

This function reads and processes a commercial SomaLogic \`.adat\` file.
It extracts RFU (Relative Florecent Units) data for samples and
controls, along with their respective metadata and feature (protein)
metadata. The function returns a structured list suitable for further
analysis.

## Usage

``` r
read_somalogic(filepath, return_Metaboprep = FALSE)
```

## Arguments

- filepath:

  A string specifying the path to the SomaLogic \`.adat\` file.

- return_Metaboprep:

  logical, if TRUE (default) return a Metaboprep object, if FALSE return
  a list.

## Value

A metaboprep object or a named list with the following elements:

- data:

  A matrix of RFU values for experimental samples, with \`SampleId\` as
  row names and \`SeqId\` (from columns containing "seq") as column
  names.

- samples:

  A \`tibble\` containing metadata for experimental samples, with
  \`sample_id\` (renamed from \`SampleId\`) as the first column.

- features:

  A \`tibble\` containing feature-level metadata (e.g., protein
  details), including a newly created \`feature_id\` column derived from
  \`SeqId\`.

- controls:

  A matrix of RFU values for control samples (specifically "Calibrator"
  samples), with \`SampleId\` as row names and \`SeqId\` as column
  names.

- control_metadata:

  A \`tibble\` containing metadata for control samples (specifically
  "Calibrator" samples), with \`sample_id\` as the first column.

## Details

The function performs several validation steps and data transformations:

- It first checks if the provided \`filepath\` points to an \`.adat\`
  file.

- It uses \`SomaDataIO::read_adat()\` to import the raw data and
  \`SomaDataIO::is.soma_adat()\` to verify its integrity as a SomaLogic
  object.

- It confirms the presence of the crucial \`SampleId\` column.

- Data is separated into experimental "Sample" and "Calibrator" control
  groups based on the \`SampleType\` column.

- For both sample and control data, RFU values corresponding to "seq"
  columns are extracted and reshaped into wide matrices with
  \`SampleId\` as row names.

- Feature metadata is extracted from \`attr(df, "Col.Meta")\`, and a new
  \`feature_id\` column is created (prefixed with "seq." and hyphens
  replaced by periods).

- Sample and control metadata are extracted from \`attr(df,
  "row_meta")\`, converted to \`tibble\`s, renamed, and \`sample_id\` is
  relocated to the front. Explicit \`tibble::as_tibble()\` and
  \`tibble::remove_rownames()\` are used to handle potential
  \`SomaDataIO\` object intricacies.

## Examples

``` r
if (FALSE) { # \dontrun{
  filepath <- system.file("extdata", "example_data10.adat", package = "SomaDataIO")
  somalogic_data <- read_somalogic(filepath)
} # }
```
