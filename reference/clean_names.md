# Standardize Column or Feature Names

Cleans a character vector of names by replacing spaces with underscores,
removing special characters (\`-\`, \`.\`), replacing \` and converting
to lowercase.

## Usage

``` r
clean_names(names)
```

## Arguments

- names:

  \`character vector\` A vector of names to be cleaned.

## Value

\`character vector\` A standardized version of the input names.

## Examples

``` r
clean_names(c("Sample ID", "Feature-Name.1", "Concentration %"))
#> [1] "sample_id"         "featurename_1"     "concentration_pct"
# Returns: c("sample_id", "featurename1", "concentration_pct")
```
