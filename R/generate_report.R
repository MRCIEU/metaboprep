#' @title Generate Output Report
#' @description
#' This function writes an output report
#' @param metaboprep an object of class Metaboprep
#' @param output_dir character, the directory to save to
#' @param output_filename character, default NULL i.e. create from input object
#' @param project character, name for the current project
#' @param format character, write either 'html' or 'pdf' report
#' @param template character, type of report to output only current option is "qc_report"
#' @include class_metaboprep.R
#' @importFrom rmarkdown render
#' @export
generate_report <- new_generic("generate_report", c("metaboprep"), function(metaboprep, output_dir, output_filename=NULL, project = "Project", format="pdf", template="qc_report") { S7_dispatch() })
#' @name generate_report
method(generate_report, Metaboprep) <- function(metaboprep, output_dir, output_filename=NULL, project = "Project", format="pdf", template="qc_report") {

  # testing
  if (FALSE) {
    output_dir="/Users/xx20081/git/metaboprep/inst/rmarkdown/templates/qc_report/skeleton"
    output_filename=NULL
    project = "Project"
    format="pdf"
    template="qc_report"
  }

  # checks
  format   <- match.arg(format, choices = c("pdf","html"))
  template <- match.arg(template, choices = available_report_templates())
  stopifnot("\n'qc' data layer not found, have you run the quality_control() function on your Metaboprep object? \n Run `dimnames(metaboprep@data)[[3]]` to see current data layers" = "dqc" %in% dimnames(metaboprep@data)[[3]])

  # name the report
  if (is.null(output_filename)) {
    outpath <- file.path(output_dir, clean_names(paste0(project, "_metaboprep_", template)))
  } else {
    outpath <- file.path(output_dir, output_filename)
  }

  # correct file extension
  if (!grepl("(?i)\\.(html|pdf)$", outpath)) {
    outpath <- paste(outpath, format, sep=".")
  }

  # ensure dir exists
  dir.create(dirname(outpath), showWarnings = FALSE, recursive = TRUE)

  # get the template
  template_path <- system.file("rmarkdown", "templates", template, "skeleton", "skeleton.Rmd", package="metaboprep")

  # render the report
  rmarkdown::render(
    input         = template_path,
    output_file   = outpath,
    params        = list(project = project, metaboprep = metaboprep),
    output_format = paste(format, "document", sep="_"),
    envir         = new.env()  # Use a new environment to avoid conflicts
  )


  #
  #
  # # Only run if the output format is PDF
  # output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  # if (output_format == "pdf") {
  #   # Check if TinyTeX is installed and install it if necessary
  #   if (!requireNamespace("tinytex", quietly = TRUE)) {
  #     message("\n\n## LaTeX Installation Instructions\n",
  #             "It seems that TinyTeX is not installed. Please follow the instructions below to install TinyTeX and the required LaTeX packages.\n\n",
  #             "### Steps to Install TinyTeX and Missing LaTeX Packages\n",
  #             "1. Install TinyTeX by running the following command in R:\n",
  #             "   ```r\n",
  #             "   install.packages('tinytex')\n",
  #             "   tinytex::install_tinytex()\n",
  #             "   ```\n",
  #             "2. Install missing LaTeX packages (e.g., 'caption'):\n",
  #             "   ```r\n",
  #             "   tinytex::tlmgr_install('caption')\n",
  #             "   ```\n",
  #             "3. If TinyTeX is installed, but packages are missing or outdated, you can update TinyTeX:\n",
  #             "   ```r\n",
  #             "   tinytex::tlmgr_update()\n",
  #             "   ```\n",
  #             "Once TinyTeX and required LaTeX packages are installed, recompile the report to generate the PDF.\n\n",
  #             "For more help on LaTeX troubleshooting, visit: https://yihui.org/tinytex/r/#debugging\n")
  #   }
  # }


  invisible(metaboprep)
}
