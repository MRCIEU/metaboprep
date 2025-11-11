# Export Data to \`MetaboAnalyst\` format

Export Data to \`MetaboAnalyst\` format

## Usage

``` r
export_metaboanalyst(metaboprep, directory, layer = NULL, group_col = NULL)
```

## Arguments

- metaboprep:

  A \`Metaboprep\` object containing the data to be exported.

- directory:

  character, string specifying the path to the directory where the data
  should be written.

- layer:

  character, the name of the \`metaboprep@data\` layer (3rd array
  dimension) to write out

- group_col:

  character, the column name in the \`metaboprep@samples\` data
  identifying the group for one-factor analysis

## Value

the \`Metaboprep\` object, invisibly, for use in pipes
