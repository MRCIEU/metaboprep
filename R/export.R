#' @title Export Data from a Metaboprep Object
#'
#' @description
#' Exports all data layers from a `Metaboprep` object to a structured directory format.
#' For each data layer, the function creates a subdirectory containing:
#' - the primary data matrix (`data.tsv`),
#' - associated feature and sample metadata (`features.tsv`, `samples.tsv`),
#' - feature and sample summaries (if present, `feature_summary.tsv`, `sample_summary.tsv`),
#' - a serialized feature tree (if present),
#' - and a `config.yml` file with additional metadata and processing parameters.
#'
#' @param metaboprep A `Metaboprep` object containing the data to be exported.
#' @param directory A character string specifying the path to the directory where the data should be written.
#'
#' @return the `Metaboprep` object, invisibly, for use in pipes
#'
#' @importFrom yaml write_yaml
#'
#' @export
export_data <- new_generic("export_data", c("metaboprep", "directory"), function(metaboprep, directory) { S7_dispatch() })
#' @name export_data
method(export_data, list(Metaboprep, class_character)) <- function(metaboprep, directory) {
  
  # make the directories
  today    <- gsub("-", "_", Sys.Date())
  dir      <- file.path(sub("/$", "", directory), paste0("metaboprep_release_", today))
  layers   <- stats::setNames(dimnames(metaboprep@data)[[3]], dimnames(metaboprep@data)[[3]])
  sub_dirs <- lapply(layers, function(x) file.path(dir, x))
  invisible(lapply(sub_dirs, dir.create, showWarnings = FALSE, recursive = TRUE))
  
  # # combine and write the data for each layer
  # for (j in seq_along(layers)) {
  #   
  #   layer <- layers[[j]]
  #   
  #   properties <- S7::props(metaboprep)
  #   
  #   # deal with feature tree separately
  #   if (layer %in% names(properties$feature_tree)) {
  #     tree_path <- file.path(sub_dirs[[layer]], "feature_tree.RDS")
  #     saveRDS(properties$feature_tree[[layer]], tree_path)
  #     properties$feature_tree[[layer]] <- tree_path
  #   }
  #   
  #   # write the yaml config
  #   config <- list(layer = j)
  #   for (i in seq_along(properties)) {
  #     if (!inherits(properties[[i]], c("data.frame", "array", "matrix"))) {
  #       if (layer %in% names(properties[[i]])) {
  #         config[[names(properties)[i]]] <- properties[[i]][[layer]]
  #       } else if (!is.null(names(properties[[i]]))) {
  #         config[[names(properties)[i]]] <- NA_character_
  #       } else if (inherits(properties[[i]], c("POSIXct"))) {
  #         config[[names(properties)[i]]] <- as.character(properties[[i]])
  #       } else {
  #         config[[names(properties)[i]]] <- properties[[i]]
  #       }
  #     }
  #   }
  #   yaml::write_yaml(config, file.path(sub_dirs[[layer]], "config.yml"))
  #   
  #   # exclusions
  #   excl_feats <- unlist(metabolites@exclusions[[layer]][["features"]])
  #   excl_samps <- unlist(metabolites@exclusions[[layer]][["samples"]])
  #   
  #   # get the features
  #   features   <- get_features(metabolites, layer=layer)
  #   incl_feats <- features[!feature_id %in% excl_feats, feature_id]
  #   
  #   # get the samples
  #   samples    <- get_samples(metabolites, layer=layer)
  #   incl_samps <- samples[!sample_id %in% excl_samps, sample_id]
  #   
  #   # write the samples
  #   data.table::fwrite(samples[sample_id %in% incl_samps], file.path(sub_dirs[[layer]], "samples.tsv"), sep="\t")
  #   
  #   # write the features
  #   data.table::fwrite(features[feature_id %in% incl_feats], file.path(sub_dirs[[layer]], "features.tsv"), sep="\t")
  #   
  #   # write the feature summary if present
  #   if (layer %in% dimnames(metabolites@feature_summary)[[3]]) {
  #     feat_sum <- t(get_feature_summary(metabolites, layer=layer, feature_ids = incl_feats))
  #     feat_sum <- cbind(
  #       data.table::data.table(feature_id = rownames(feat_sum)),
  #       data.table::as.data.table(feat_sum)
  #     )
  #     data.table::fwrite(feat_sum, file.path(sub_dirs[[layer]], "feature_summary.tsv"), sep="\t")
  #   }
  #   
  #   # write the sample summary if present
  #   if (layer %in% dimnames(metabolites@sample_summary)[[3]]) {
  #     samp_sum <- get_sample_summary(metabolites, layer=layer, sample_ids = incl_samps)
  #     samp_sum <- cbind(
  #       data.table::data.table(sample_id = rownames(samp_sum)),
  #       data.table::as.data.table(samp_sum)
  #     )
  #     data.table::fwrite(samp_sum, file.path(sub_dirs[[layer]], "sample_summary.tsv"), sep="\t")
  #   }
  #   
  #   # write the data
  #   if (layer %in% dimnames(metabolites@data)[[3]]) {
  #     data <- get_data(metabolites, layer=layer, sample_ids = incl_samps, feature_ids = incl_feats)
  #     data <- cbind(
  #       data.table::data.table(sample_id = rownames(data)),
  #       data.table::as.data.table(data)
  #     )
  #     data.table::fwrite(data, file.path(sub_dirs[[layer]], "data.tsv"), sep="\t")
  #   }
  #   
  #   # write the pcs
  #   if (layer %in% dimnames(metabolites@data)[[3]]) {
  #     pcs <- get_pcs(metabolites, layer=layer, sample_ids = incl_samps)
  #     pcs <- cbind(
  #       data.table::data.table(sample_id = rownames(pcs)),
  #       data.table::as.data.table(pcs)
  #     )
  #     data.table::fwrite(pcs, file.path(sub_dirs[[layer]], "pcs.tsv"), sep="\t")
  #   }
  #   
  #   # write the prob pcs
  #   if (layer %in% dimnames(metabolites@data)[[3]]) {
  #     prob_pcs <- get_prob_pcs(metabolites, layer=layer, sample_ids = incl_samps)
  #     prob_pcs <- cbind(
  #       data.table::data.table(sample_id = rownames(prob_pcs)),
  #       data.table::as.data.table(prob_pcs)
  #     )
  #     data.table::fwrite(prob_pcs, file.path(sub_dirs[[layer]], "prob_pcs.tsv"), sep="\t")
  #   }
  #   
  #   # write the var explained
  #   if (layer %in% dimnames(metabolites@data)[[3]]) {
  #     var_exp <- get_var_exp(metabolites, layer=layer)
  #     var_exp <- data.table::data.table(pc    = names(var_exp),
  #                                       value = var_exp)
  #     data.table::fwrite(var_exp, file.path(sub_dirs[[layer]], "var_exp.tsv"), sep="\t")
  #   }
  #   
  #   
  #   cli::cli_alert_success("Exported `{layer}` data layer to directory {.file {sub_dirs[[layer]]}}")
  # }
  
  invisible(metaboprep)
}