# Read Metabolon Data

Read Metabolon Data

## Usage

``` r
read_metabolon(
  filepath,
  sheet = NULL,
  feature_sheet = NULL,
  feature_id_col = NULL,
  sample_sheet = NULL,
  sample_id_col = NULL,
  return_Metaboprep = TRUE
)
```

## Arguments

- filepath:

  character, commercial Metabolon excel sheet with extension .xls or
  .xlsx

- sheet:

  character or integer, the excel sheet name (or index) from which to
  read.

- feature_sheet:

  character or integer, the excel sheet name (or index) from which to
  read the feature data.

- feature_id_col:

  character, the excel column containing the feature_id mapping to the
  data.

- sample_sheet:

  character or integer, the excel sheet name (or index) from which to
  read the sample data.

- sample_id_col:

  character, the excel column containing the sample_id mapping to the
  data.

- return_Metaboprep:

  logical, if TRUE (default) return a Metaboprep object, if FALSE return
  a list.

## Value

list or Metaboprep object, list(data = matrix, samples = samples
data.frame, features = features data.frame)

## Examples

``` r
# version 1.1 data format
filepath1 <- system.file("extdata", "metabolon_v1.1_example.xlsx", package = "metaboprep")
m <- read_metabolon(filepath1, sheet = 2)

# version 1.2 data format (different column names)
filepath2 <- system.file("extdata", "metabolon_v1.2_example.xlsx", package = "metaboprep")
m <- read_metabolon(filepath2, sheet = 'OrigScale')


# version 2 data format
filepath3 <- system.file("extdata", "metabolon_v2_example.xlsx", package = "metaboprep")
m <- read_metabolon(filepath3, 
                    sheet = 'Batch-normalized Data', 
                    feature_sheet = 'Chemical Annotation', 
                    feature_id_col = 'CHEM_ID', 
                    sample_sheet = 'Sample Meta Data', 
                    sample_id_col = 'PARENT_SAMPLE_NAME')
```
