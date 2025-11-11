# Export Data from a Metaboprep Object

Exports all data from a \`Metaboprep\` object to a structured directory
format. For each data layer, the function creates a subdirectory
containing: - the primary data matrix (\`data.tsv\`), - associated
feature and sample metadata (\`features.tsv\`, \`samples.tsv\`), -
feature and sample summaries (if present, \`feature_summary.tsv\`,
\`sample_summary.tsv\`), - a serialized feature tree (if present), - and
a \`config.yml\` file with additional metadata and processing
parameters.

## Usage

``` r
export(metaboprep, directory, format = "metaboprep", ...)
```

## Arguments

- metaboprep:

  A \`Metaboprep\` object containing the data to be exported.

- directory:

  character, string specifying the path to the directory where the data
  should be written.

- format:

  character, string specifying the format of the exported data - one of
  "metaboprep", "comets", or "metaboanalyst".

- ...:

  Arguments passed on to
  [`export_comets`](https://mrcieu.github.io/metaboprep/reference/export_comets.md),
  [`export_metaboanalyst`](https://mrcieu.github.io/metaboprep/reference/export_metaboanalyst.md)

  `layer`

  :   character, the name of the \`metaboprep@data\` layer (3rd array
      dimension) to write out

  `group_col`

  :   character, the column name in the \`metaboprep@samples\` data
      identifying the group for one-factor analysis

## Value

the \`Metaboprep\` object, invisibly, for use in pipes
