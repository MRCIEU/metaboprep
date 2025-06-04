#' @title Feature Summary Statistics
#' @description
#' This function estimates feature statistics for samples in a matrix of metabolite features.
#' @param metabolites an object of class Metabolites
#' @param layer character, the data type/source
#' @param sample_ids character, vector of sample ids
#' @param feature_ids character, vector of feature ids
#' @param output character, the output format options: "object", "data.table", "matrix", "list"
#' @include class_metaboprep.R
#' @export
feature_summary <- new_generic("feature_summary", c("metabolites"), function(metabolites, layer="raw", feature_ids=NULL, sample_ids=NULL, output="object") { S7_dispatch() })
#' @name feature_summary
method(feature_summary, Metaboprep) <- function(metabolites, layer="raw", feature_ids=NULL, sample_ids=NULL, output="object") {

  # options
  output <- match.arg(output, choices = c("object", "data.table", "matrix", "list"))

  # the data to work with
  dat <- get_data(metabolites, layer=layer, feature_ids=feature_ids, sample_ids=sample_ids)

  ## feature missingness
  featuremis = missingness(dat, by="column", exclude_features = NA)

  ### distribution discritions
  description <- feature_describe(dat) |> data.table::as.data.table(keep.rownames = TRUE)
  data.table::setnames(description, "rn", "feature_id")

  ### count of sample outliers per feature
  omat     <- outlier_detection(dat, nsd = metabolites@outlier_udist, meansd = FALSE, by = "column")
  sumomat  <- apply(omat, 2, sum)
  outliers <- data.table::data.table(feature_id    = names(sumomat),
                                     outlier_count = sumomat)

  ##    FEATURES MUST HAVE > 80% Presence or <= 20% missing
  min_sample_size = floor( nrow(dat) * 0.8 )

  # features to exclude
  exclude_features <- character()
  if (metabolites@derived_var_exclusion) {
    exclude_features <- metabolites@features[derived_feature==TRUE, feature_id] # derived
  }

  ##
  indf = tree_and_independent_features(dat,
                                       minimum_samplesize = min_sample_size,
                                       tree_cut_height    = metabolites@tree_cut_height,
                                       exclude_features   = exclude_features )

  # combine summary data
  dt_list <- list(featuremis, outliers, description, indf$dat)
  out <- Reduce(function(x, y) data.table::merge.data.table(x, y, by = "feature_id", all = TRUE), dt_list)

  # ensure correct order (use the unfiltered data to get the col names, inject NAs if absent from filtered data)
  ordered_base_ids <- data.table::data.table(feature_id = colnames(metabolites@data[, , layer]))
  out <- out[ordered_base_ids, on="feature_id", nomatch = NA]

  # as matrix
  mat <- as.matrix(out[, !("feature_id")])
  rownames(mat) <- out$feature_id
  mat <- t(mat) # keep convension that features are in columns

  # add to sample_summary matrix
  metabolites@feature_summary <- add_layer(current    = metabolites@feature_summary,
                                           layer      = mat,
                                           layer_name = layer)

  # add tree to object
  metabolites@feature_tree[[layer]] <- indf$speartree

  # return desired output
  return(
    switch(output,
           "object"     = metabolites,
           "data.table" = out,
           "matrix"     = mat,
           "list"       = list(table = out, tree = indf$speartree)
    )
  )
}

