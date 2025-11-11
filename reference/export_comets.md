# Export Data to \`COMETS\` format

Export Data to \`COMETS\` format

## Usage

``` r
export_comets(metaboprep, directory, layer = NULL)
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

## Value

the \`Metaboprep\` object, invisibly, for use in pipes
