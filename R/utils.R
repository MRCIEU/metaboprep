#' @title List Available Data Formats
#' @description Scans the package source files for functions starting with "read_" to determine supported data formats.
#' @return A named character vector of available data formats.
#' @export
available_data_formats <- function() {
  r_dir <- system.file(package = "metaboprep")
  if (grepl("inst$", r_dir)) {
    r_dir <- sub("/inst","",r_dir) # local package development
  }
  namespace_file <- list.files(r_dir, pattern = "NAMESPACE", full.names = TRUE)
  lines <- readLines(namespace_file, warn = FALSE)
  read_functions <- unique(grep("^export\\(read_[a-zA-Z0-9_]+\\)", lines, value = TRUE))
  formats <- sub("^export\\(read_([a-zA-Z0-9_]+)\\)", "\\1", read_functions)
  return(formats)
}



#' @title List Available Report Templates
#' @description Scans the package source files for available report templates to write to.
#' @return A character vector of available report templates
#' @export
available_report_templates <- function() {
  r_dir <- system.file(package = "metaboprep")
  templates <- list.dirs(file.path(r_dir, "rmarkdown", "templates"), recursive = FALSE, full.names = FALSE)
  return(templates)
}


#' @title Standardize Column or Feature Names
#' @description
#' Cleans a character vector of names by replacing spaces with underscores,
#' removing special characters (`-`, `.`), replacing `%` with `pct`,
#' and converting to lowercase.
#' @param names `character vector` A vector of names to be cleaned.
#' @return `character vector` A standardized version of the input names.
#' @export
#' @examples
#' clean_names(c("Sample ID", "Feature-Name.1", "Concentration %"))
#' # Returns: c("sample_id", "featurename1", "concentration_pct")
clean_names <- function(names) {
  n <- gsub("[\\. ]", "_", names)    # Replace spaces with underscores
  n <- gsub("[-/]", "", n)   # Remove dashes and dots
  n <- gsub("%", "pct", n)      # Replace "%" with "pct"
  tolower(n)                    # Convert to lowercase
}

