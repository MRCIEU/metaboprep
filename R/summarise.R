#' @title Summary Statistics
#' @description
#' Summarise the sample and feature data
#' @inheritParams sample_summary
#' @inheritParams feature_summary
#' @param output character, type of output, either 'object' to return the updated metaboprep object, or 'data.frame' to return the data.
#' @include class_metaboprep.R
#' @export
summarise <- new_generic(name = "summarise", dispatch_args = c("metaboprep"), function(metaboprep, source_layer="input", outlier_udist=5, tree_cut_height=0.5, sample_ids=NULL, feature_ids=NULL, output="data.frame") { S7_dispatch() })
#' @name summarise
method(summarise, Metaboprep) <- function(metaboprep, source_layer="input", outlier_udist=5, tree_cut_height=0.5, sample_ids=NULL, feature_ids=NULL, output="data.frame") {
  
  # check inputs 
  output       <- match.arg(output, choices = c("object", "data.frame"))
  source_layer <- match.arg(source_layer, choices = dimnames(metaboprep@data)[[3]])
  stopifnot("sample_ids must all be found in the data" = all(sample_ids %in% metaboprep@samples[["sample_id"]]) | is.null(sample_ids))
  stopifnot("feature_ids must all be found in the data" = all(feature_ids %in% metaboprep@features[["feature_id"]]) | is.null(feature_ids))  
  
  
  # get ids
  if (is.null(sample_ids)) sample_ids   <- metaboprep@samples[["sample_id"]]
  if (is.null(feature_ids)) feature_ids <- metaboprep@features[["feature_id"]]
  
  
  # run summaries
  feature_sum <- feature_summary(metaboprep, source_layer=source_layer, outlier_udist=outlier_udist, tree_cut_height=tree_cut_height, sample_ids=sample_ids, feature_ids=feature_ids, output="data.frame")
  sample_sum  <- sample_summary(metaboprep,  source_layer=source_layer, outlier_udist=outlier_udist, sample_ids=sample_ids, feature_ids=feature_ids, output="data.frame")
  indep_feats <- feature_sum[feature_sum$independent_features & !is.na(feature_sum$independent_features), "feature_id"]
  pc_outlier  <- pc_and_outliers(metaboprep, source_layer=source_layer, sample_ids=sample_ids, feature_ids=indep_feats)
  sample_sum  <- merge(sample_sum, pc_outlier, by="sample_id", all = TRUE)
  sample_sum  <- sample_sum[order(match(sample_sum[["sample_id"]], rownames(metaboprep@data))), ]
  rownames(sample_sum) <- sample_sum[["sample_id"]]
  
  # return desired output
  return(
    switch(output,
           "object"     = {
             # set feature summary
             feature_sum_mat <- as.matrix(feature_sum[, !(names(feature_sum) %in% "feature_id")])
             feature_sum_mat <- t(feature_sum_mat)
             metaboprep@feature_summary <- add_layer(current    = metaboprep@feature_summary,
                                                     layer      = feature_sum_mat,
                                                     layer_name = source_layer, force=TRUE)
             attr(metaboprep@feature_summary, paste0(source_layer, "_outlier_udist")) <- outlier_udist
             attr(metaboprep@feature_summary, paste0(source_layer, "_tree_cut_height")) <- tree_cut_height
             
             # set sample summary
             sample_sum_mat <- as.matrix(sample_sum[, !(names(sample_sum) %in% "sample_id")])
             metaboprep@sample_summary <- add_layer(current    = metaboprep@sample_summary,
                                                    layer      = sample_sum_mat,
                                                    layer_name = source_layer, force=TRUE)
             attr(metaboprep@sample_summary, paste0(source_layer, "_outlier_udist")) <- outlier_udist
             metaboprep
           },
           "data.frame" = list(sample_summary  = sample_sum, 
                               feature_summary = feature_sum) 
    )
  )
  
}