#' @title Feature Summary Statistics
#' @description
#' This function estimates feature statistics for samples in a matrix of metabolite features.
#' @param metaboprep an object of class Metabolites
#' @param source_layer character, the data layer to summarise
#' @param outlier_udist the unit distance in SD or IQR from the mean or median estimate, respectively outliers are identified at. Default value is 5.
#' @param tree_cut_height numeric, the threshold for feature independence in hierarchical clustering. Default is 0.5.
#' @param feature_selection character, either 'max_var_exp' or 'least_missingness', how to select the independent feature within clusters
#' @param sample_ids character, vector of sample ids to work with
#' @param feature_ids character, vector of feature ids to work with
#' @param features_exclude character, vector of feature id indicating features to exclude from the sample and PCA summary analysis but keep in the data
#' @param output character, type of output, either 'object' to return the updated metaboprep object, or 'data.frame' to return the data.
#' @include class_metaboprep.R
#' @export
feature_summary <- new_generic(name = "feature_summary", dispatch_args = c("metaboprep"), function(metaboprep, source_layer="input", outlier_udist=5, tree_cut_height=0.5, feature_selection = "max_var_exp", sample_ids=NULL, feature_ids=NULL, features_exclude=NULL, output="data.frame") { S7_dispatch() })
#' @name feature_summary
method(feature_summary, Metaboprep) <- function(metaboprep, source_layer="input", outlier_udist=5, tree_cut_height=0.5, feature_selection = "max_var_exp", sample_ids=NULL, feature_ids=NULL, features_exclude=NULL, output="data.frame") {

  # options
  output       <- match.arg(output, choices = c("object", "matrix", "data.frame"))
  source_layer <- match.arg(source_layer, choices = dimnames(metaboprep@data)[[3]])
  stopifnot("sample_ids must all be found in the data" = all(sample_ids %in% metaboprep@samples[["sample_id"]]) | is.null(sample_ids))
  stopifnot("feature_ids must all be found in the data" = all(feature_ids %in% metaboprep@features[["feature_id"]]) | is.null(feature_ids))  
  stopifnot("features_exclude must all be found in the data" = all(features_exclude %in% metaboprep@features[["feature_id"]]) | is.null(features_exclude)) 
  
  
  # get ids
  if (is.null(sample_ids)) sample_ids   <- metaboprep@samples[["sample_id"]]
  if (is.null(feature_ids)) feature_ids <- metaboprep@features[["feature_id"]]
  
  
  # the data to work with
  dat <- metaboprep@data[sample_ids, feature_ids, source_layer]
  
  
  ## feature missingness
  featuremis <- missingness(dat, by="column")

  
  # distribution discritions
  description <- feature_describe(dat)


  # count of sample outliers per feature
  omat     <- outlier_detection(dat, nsd = outlier_udist, meansd = FALSE, by = "column")
  sumomat  <- apply(omat, 2, sum)
  outliers <- data.frame("feature_id"    = names(sumomat),
                         "outlier_count" = sumomat)

  
  # features must have > 80% presence or <= 20% missing
  remove <- featuremis[featuremis$missingness > 0.2, "feature_id"]
  if(length(remove) > 0){
    dat <- dat[, !(colnames(dat) %in% remove), drop = FALSE]
  }
  

  # do the tree cut
  res  <- tree_and_independent_features(dat, tree_cut_height = tree_cut_height, features_exclude = features_exclude, feature_selection = feature_selection)
  indf <- res$data
  
  
  # combine summary data
  ids     <- data.frame("feature_id" = colnames(metaboprep@data))
  df_list <- list(ids, featuremis, outliers, description, indf)
  out     <- Reduce(function(x, y) merge(x, y, by = "feature_id", all = TRUE), df_list)
  
  
  # ensure correct order
  out <- out[order(match(out[["feature_id"]], colnames(metaboprep@data))), ]
  rownames(out) <- out[["feature_id"]]
  
  
  # add to feature_summary matrix
  mat           <- as.matrix(out[, !(names(out) %in% "feature_id")])
  mat           <- t(mat)
  metaboprep@feature_summary <- add_layer(current    = metaboprep@feature_summary,
                                          layer      = mat,
                                          layer_name = source_layer)
  

  # set attributes with processing details & return desired output
  ret <- switch(output,
                "object"     = {
                  attr(metaboprep@feature_summary, paste0(source_layer, "_tree")) <- res$tree
                  attr(metaboprep@feature_summary, paste0(source_layer, "_outlier_udist")) <- outlier_udist
                  attr(metaboprep@feature_summary, paste0(source_layer, "_tree_cut_height")) <- tree_cut_height
                  metaboprep
                },
                "matrix"     = {
                  attr(mat, paste0(source_layer, "_tree"))            <- res$tree
                  attr(mat, paste0(source_layer, "_outlier_udist"))   <- outlier_udist
                  attr(mat, paste0(source_layer, "_tree_cut_height")) <- tree_cut_height
                  mat
                },
                "data.frame" = {
                  attr(out, paste0(source_layer, "_tree"))            <- res$tree
                  attr(out, paste0(source_layer, "_outlier_udist"))   <- outlier_udist
                  attr(out, paste0(source_layer, "_tree_cut_height")) <- tree_cut_height
                  out
                })
  
  return(ret)
    
}

