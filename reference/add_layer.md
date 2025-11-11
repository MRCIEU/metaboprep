# Add a Layer of Data (internal use)

This function adds an additional layer of data along the third dimension
to an existing 3D array (or 2D matrix/vector) by stacking a new layer of
data. It ensures that the dimensions of the new layer match the first
two dimensions of the existing array or matrix. If there is a mismatch
in row or column names and the \`force\` parameter is set to \`TRUE\`,
the function will align the data by filling missing values with \`NA\`.
It is used internally and not intended for routine user use.

## Usage

``` r
add_layer(current, layer, layer_name, force = FALSE)
```

## Arguments

- current:

  A vector, matrix, or 3D array representing the current stack of data.

- layer:

  A matrix or array that represents the new layer of data to be added.
  It should match the dimensions of the first two dimensions of
  \`current\`.

- layer_name:

  A character string specifying the name of the new dimension for the
  3rd axis. This can be used to annotate the new data layer.

- force:

  A logical value indicating whether to force the join and create \`NA\`
  values where row or column names do not match between \`current\` and
  \`layer\`. Default is \`FALSE\`.

## Value

A 3D array with the added layer in the third dimension.
