#' @title Sample Summary Statistics
#' @description
#' Summarise the sample data
#' @param metaboprep an object of class Metaboprep
#' @param source_layer character, the data layer to summarise
#' @param outlier_udist the unit distance in SD or IQR from the mean or median estimate, respectively outliers are identified at. Default value is 5.
#' @param sample_ids character, vector of sample ids to work with
#' @param feature_ids character, vector of feature ids to work with
#' @param output character, type of output, either `object` to return the updated metaboprep object, or data.frame to return the data.
#' @include class_metaboprep.R
#' @export
sample_summary <- new_generic(name = "sample_summary", dispatch_args = c("metaboprep"), function(metaboprep, source_layer="input", outlier_udist=5, exclude_derived=FALSE, exclude_xenobiotics=FALSE, sample_ids=NULL, feature_ids=NULL, output="object") { S7_dispatch() })
#' @name sample_summary
method(sample_summary, Metaboprep) <- function(metaboprep, source_layer="input", outlier_udist=5, exclude_derived=FALSE, exclude_xenobiotics=FALSE, sample_ids=NULL, feature_ids=NULL, output="object") {

  # check inputs 
  output       <- match.arg(output, choices = c("object", "data.frame"))
  source_layer <- match.arg(source_layer, choices = dimnames(metaboprep@data)[[3]])
  stopifnot("sample_ids must all be found in the data" = all(sample_ids %in% metaboprep@samples[["sample_id"]]) | is.null(sample_ids))
  stopifnot("feature_ids must all be found in the data" = all(feature_ids %in% metaboprep@features[["feature_id"]]) | is.null(feature_ids))  
  
  
  # if feature_ids/sample_ids NULL use current exclusions
  if (is.null(sample_ids)) sample_ids   <- setdiff(metaboprep@samples[["sample_id"]], unlist(metaboprep@exclusions[["samples"]]))
  if (is.null(feature_ids)) feature_ids <- setdiff(metaboprep@features[["feature_id"]], unlist(metaboprep@exclusions[["features"]]))
  
  
  # exclude derived or xenobiotics if requested
  if (exclude_derived)     feature_ids <- setdiff(feature_ids, metaboprep@features[derived_feature==TRUE, "feature_id"])
  if (exclude_xenobiotics) feature_ids <- setdiff(feature_ids, metaboprep@features[xenobiotic_feature==TRUE, "feature_id"])
 
  
   # the data to work with
  dat <- metaboprep@data[sample_ids, feature_ids, source_layer]

  
  # missingness
  missing <- missingness(dat, by="row")

  
  # total peak area
  tpa <- total_peak_area(dat)

  
  # count sample outliers
  omat     <- outlier_detection(dat, nsd = outlier_udist, meansd = FALSE, by = "column")
  sumomat  <- apply(omat, 1, sum)
  outliers <- data.frame("sample_id"     = names(sumomat),
                         "outlier_count" = sumomat)

  
  # combine
  ids     <- data.frame("sample_id" = rownames(metaboprep@data))
  df_list <- list(ids, missing, tpa, outliers)
  out     <- Reduce(function(x, y) merge(x, y, by = "sample_id", all = TRUE), df_list)

  
  # ensure correct order
  out <- out[order(match(out[["sample_id"]], rownames(metaboprep@data))), ]

  
  # as matrix
  mat <- as.matrix(out[, !(names(out) %in% "sample_id")])
  rownames(mat) <- out$sample_id

  
  # add to sample_summary matrix
  metaboprep@sample_summary <- add_layer(current    = metaboprep@sample_summary,
                                         layer      = mat,
                                         layer_name = source_layer)


  # return desired output
  return(
    switch(output,
           "object"     = metaboprep,
           "data.frame" = out
    )
  )
}


