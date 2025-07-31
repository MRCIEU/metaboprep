#' @title Summary Statistics
#' @description
#' Summarise the sample and feature data
#' @inheritParams sample_summary
#' @inheritParams feature_summary
#' @param output character, type of output, either 'object' to return the updated metaboprep object, or 'data.frame' to return the data.
#' @include class_metaboprep.R
#' @export
summarise <- new_generic(name = "summarise", dispatch_args = c("metaboprep"), function(metaboprep, source_layer="input", outlier_udist=5, tree_cut_height=0.5, feature_selection = "max_var_exp", sample_ids=NULL, feature_ids=NULL, features_exclude=NULL, output="data.frame") { S7_dispatch() })
#' @name summarise
method(summarise, Metaboprep) <- function(metaboprep, source_layer="input", outlier_udist=5, tree_cut_height=0.5, feature_selection = "max_var_exp", sample_ids=NULL, feature_ids=NULL, features_exclude=NULL, output="data.frame") {
  
  # check inputs 
  output       <- match.arg(output, choices = c("object", "data.frame"))
  source_layer <- match.arg(source_layer, choices = dimnames(metaboprep@data)[[3]])
  stopifnot("sample_ids must all be found in the data" = all(sample_ids %in% metaboprep@samples[["sample_id"]]) | is.null(sample_ids))
  stopifnot("feature_ids must all be found in the data" = all(feature_ids %in% metaboprep@features[["feature_id"]]) | is.null(feature_ids))  
  stopifnot("features_exclude must all be found in the data" = all(features_exclude %in% metaboprep@features[["feature_id"]]) | is.null(features_exclude)) 
  
  
  # get ids
  if (is.null(sample_ids)) sample_ids   <- metaboprep@samples[["sample_id"]]
  if (is.null(feature_ids)) feature_ids <- metaboprep@features[["feature_id"]]
  
  
  # run summaries
  feature_sum <- feature_summary(metaboprep, 
                                 source_layer     = source_layer, 
                                 outlier_udist    = outlier_udist, 
                                 tree_cut_height  = tree_cut_height,
                                 feature_selection= feature_selection,
                                 sample_ids       = sample_ids, 
                                 feature_ids      = feature_ids,
                                 features_exclude = features_exclude, 
                                 output           = "data.frame")
  
  sample_sum  <- sample_summary(metaboprep,  
                                source_layer  = source_layer, 
                                outlier_udist = outlier_udist, 
                                sample_ids    = sample_ids, 
                                feature_ids   = setdiff(feature_ids, features_exclude),
                                output        = "data.frame")
  
  indep_feats <- feature_sum[feature_sum$independent_features & !is.na(feature_sum$independent_features), "feature_id"]
  
  pc_outlier  <- pc_and_outliers(metaboprep, 
                                 source_layer = source_layer, 
                                 sample_ids   = sample_ids, 
                                 feature_ids  = indep_feats)
  
  sample_sum  <- merge(sample_sum, pc_outlier, by="sample_id", all = TRUE)
  sample_sum  <- sample_sum[order(match(sample_sum[["sample_id"]], rownames(metaboprep@data))), ]
  rownames(sample_sum) <- sample_sum[["sample_id"]]
  
  # keep attributes
  pc_attrs <- attributes(pc_outlier)
  for (att in setdiff(names(pc_attrs), c("row.names", "names", "class"))) {
    attr(sample_sum, att) <- pc_attrs[[att]]
  }
  attr(sample_sum, paste0(source_layer, "_outlier_udist")) <- outlier_udist
  
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
             # keep attributes
             feat_attrs <- attributes(feature_sum)
             for (att in setdiff(names(feat_attrs), c("row.names", "names", "class"))) {
               attr(metaboprep@feature_summary, att) <- feat_attrs[[att]]
             }
             
             # set sample summary
             sample_sum_mat <- as.matrix(sample_sum[, !(names(sample_sum) %in% "sample_id")])
             metaboprep@sample_summary <- add_layer(current    = metaboprep@sample_summary,
                                                    layer      = sample_sum_mat,
                                                    layer_name = source_layer, force=TRUE)
             # keep attributes
             samp_attrs <- attributes(sample_sum)
             for (att in setdiff(names(samp_attrs), c("row.names", "names", "class"))) {
               attr(metaboprep@sample_summary, att) <- samp_attrs[[att]]
             }
             metaboprep
           },
           "data.frame" = list(sample_summary  = sample_sum, 
                               feature_summary = feature_sum) 
    )
  )
  
}