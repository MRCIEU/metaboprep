#' @title Summary Method for Metaboprep Object
#' @description
#' Provides a concise, human-readable summary of a `Metaboprep` object.
#' It reports key dimensions of the data, the presence of metadata columns,
#' the number of data layers, and the status of quality control summaries and exclusions.
#'
#' @param object A `Metaboprep` object.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns NULL. Prints a formatted summary to the console.
#'
#' @seealso \code{\link{Metaboprep-class}}
#' @importFrom S7 method S7_inherits
#' @name summary.Metaboprep
S7::method(summary, Metaboprep) <- function(object, ...) {
  # stopifnot(S7::S7_inherits(object, Metaboprep))
  
  output <- c()
  
  output <- c(output, "Metaboprep Object Summary", "--------------------------")
  
  # Basic structure
  output <- c(output,
              paste("Samples      :", nrow(object@samples)),
              paste("Features     :", nrow(object@features)),
              paste("Data Layers  :", if (length(dim(object@data)) == 3) length(dimnames(object@data)[[3]]) else 1),
              paste("Layer Names  :", paste(dimnames(object@data)[[3]], collapse = ", ")),
              "")
  
  # Summary layers
  output <- c(output,
              paste("Sample Summary Layers :", if (length(object@sample_summary) > 0) paste(dimnames(object@sample_summary)[[3]], collapse = ", ") else "none"),
              paste("Feature Summary Layers:", if (length(object@feature_summary) > 0) paste(dimnames(object@feature_summary)[[3]], collapse = ", ") else "none"),
              "")
  
  # Sample and Feature Metadata
  output <- c(output, "Sample Annotation (metadata):")
  output <- c(output,
              paste0("  Columns: ", ncol(object@samples)),
              paste0("  Names  : ", paste(names(object@samples), collapse = ", ")),
              "")
  
  output <- c(output, "Feature Annotation (metadata):")
  output <- c(output,
              paste0("  Columns: ", ncol(object@features)),
              paste0("  Names  : ", paste(names(object@features), collapse = ", ")),
              "")
  
  # Exclusion Codes Summary
  output <- c(output, "Exclusion Codes Summary:")
  
  # Helper to format exclusion tables as aligned character rows
  format_exclusion_df <- function(df) {
    formatted <- format(df, justify = "left")
    header <- paste(names(formatted), collapse = " | ")
    separator <- paste(rep("-", nchar(header)), collapse = "")
    body <- apply(formatted, 1, paste, collapse = " | ")
    c(header, separator, body)
  }
  
  # Sample exclusions
  if (!is.null(object@exclusions$samples) && length(object@exclusions$samples) > 0) {
    df_sample <- data.frame(
      Exclusion = names(object@exclusions$samples),
      Count = sapply(object@exclusions$samples, length),
      row.names = NULL
    )
    output <- c(output, "", "  Sample Exclusions:")
    output <- c(output, format_exclusion_df(df_sample))
  } else {
    output <- c(output, "  No sample exclusions.")
  }
  
  # Feature exclusions
  if (!is.null(object@exclusions$features) && length(object@exclusions$features) > 0) {
    df_feature <- data.frame(
      Exclusion = names(object@exclusions$features),
      Count = sapply(object@exclusions$features, length),
      row.names = NULL
    )
    output <- c(output, "", "  Feature Exclusions:")
    output <- c(output, format_exclusion_df(df_feature))
  } else {
    output <- c(output, "  No feature exclusions.")
  }
  
  # Print all at once
  cat(paste(output, collapse = "\n"), "\n")
  
  invisible(NULL)
}
