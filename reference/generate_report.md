# Generate Output Report

This function writes an output report

## Usage

``` r
generate_report(
  metaboprep,
  output_dir,
  output_filename = NULL,
  project = "Project",
  format = "pdf",
  template = "qc_report"
)
```

## Arguments

- metaboprep:

  an object of class Metaboprep

- output_dir:

  character, the directory to save to

- output_filename:

  character, default NULL i.e. create from input object

- project:

  character, name for the current project

- format:

  character, write either 'html' or 'pdf' report

- template:

  character, type of report to output only current option is "qc_report"
