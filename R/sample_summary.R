#' @title Sample Summary Statistics
#' @description
#' Summarise the sample data
#' @param metaboprep an object of class Metaboprep
#' @param source_layer character, the data layer to summarise
#' @param outlier_udist the unit distance in SD or IQR from the mean or median estimate, respectively outliers are identified at. Default value is 5.
#' @param sample_ids character, vector of sample ids to work with
#' @param feature_ids character, vector of feature ids to work with
#' @param output character, type of output, either 'object' to return the updated metaboprep object, or 'data.frame' to return the data.
#' @include class_metaboprep.R
#' @export
sample_summary <- new_generic(name = "sample_summary", dispatch_args = c("metaboprep"), function(metaboprep, source_layer="input", outlier_udist=5, sample_ids=NULL, feature_ids=NULL, output="data.frame") { S7_dispatch() })
#' @name sample_summary
method(sample_summary, Metaboprep) <- function(metaboprep, source_layer="input", outlier_udist=5, sample_ids=NULL, feature_ids=NULL, output="data.frame") {

  # check inputs 
  output       <- match.arg(output, choices = c("object", "matrix", "data.frame"))
  source_layer <- match.arg(source_layer, choices = dimnames(metaboprep@data)[[3]])
  stopifnot("sample_ids must all be found in the data" = all(sample_ids %in% metaboprep@samples[["sample_id"]]) | is.null(sample_ids))
  stopifnot("feature_ids must all be found in the data" = all(feature_ids %in% metaboprep@features[["feature_id"]]) | is.null(feature_ids))  
  
  
  # get ids
  if (is.null(sample_ids)) sample_ids   <- metaboprep@samples[["sample_id"]]
  if (is.null(feature_ids)) feature_ids <- metaboprep@features[["feature_id"]]
  
  
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
  rownames(out) <- out[["sample_id"]]

  
  # as matrix
  mat <- as.matrix(out[, !(names(out) %in% "sample_id")])

  
  # add to sample_summary matrix
  metaboprep@sample_summary <- add_layer(current    = metaboprep@sample_summary,
                                         layer      = mat,
                                         layer_name = source_layer, 
                                         force      = TRUE)


  # set attributes with processing details & return desired output
  ret <- switch(output,
                "object"     = {
                  attr(metaboprep@sample_summary, paste0(source_layer, "_outlier_udist")) <- outlier_udist
                  metaboprep
                },
                "matrix"     = {
                  attr(mat, paste0(source_layer, "_outlier_udist"))   <- outlier_udist
                  mat
                },
                "data.frame" = {
                  attr(out, paste0(source_layer, "_outlier_udist"))   <- outlier_udist
                  out
                })
  
  return(ret)
}


