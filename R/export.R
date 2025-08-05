#' @title Export Data from a Metaboprep Object
#'
#' @description
#' Exports all data from a `Metaboprep` object to a structured directory format.
#' For each data layer, the function creates a subdirectory containing:
#' - the primary data matrix (`data.tsv`),
#' - associated feature and sample metadata (`features.tsv`, `samples.tsv`),
#' - feature and sample summaries (if present, `feature_summary.tsv`, `sample_summary.tsv`),
#' - a serialized feature tree (if present),
#' - and a `config.yml` file with additional metadata and processing parameters.
#' 
#' @inheritDotParams export_comets layer
#' @inheritDotParams export_metaboanalyst something
#' @param metaboprep A `Metaboprep` object containing the data to be exported.
#' @param directory character, string specifying the path to the directory where the data should be written.
#' @param format character, string specifying the format of the exported data - one of "metaboprep", "comets", or "metaboanalyst".
#' @param ... other parameters passed to \code{export_metaboprep()}, \code{export_comets()}, or \code{export_metaboanalyst()}.
#'
#' @return the `Metaboprep` object, invisibly, for use in pipes
#'
#' @export
export <- new_generic("export", c("metaboprep", "directory"), function(metaboprep, directory, format = "metaboprep", ...) { S7_dispatch() })
#' @name export
method(export, list(Metaboprep, class_character)) <- function(metaboprep, directory, format = "metaboprep", ...) {
  
  format <- match.arg(format, choices = c("metaboprep", "comets", "metaboanalyst"))
  
  switch (format,
    "metaboprep"    = export_metaboprep(metaboprep, directory, ...), 
    "comets"        = export_comets(metaboprep, directory, ...),
    "metaboanalyst" = export_metaboanalyst(metaboprep, directory, ...)
  )
  
  invisible(metaboprep)
}


#' @title Export Data to `Metaboprep` format
#' @inheritParams export
#' @return the `Metaboprep` object, invisibly, for use in pipes
#' 
#' @importFrom yaml write_yaml 
#' 
#' @export
export_metaboprep <- new_generic("export_metaboprep", c("metaboprep", "directory"), function(metaboprep, directory, ...) { S7_dispatch() })
#' @name export_metaboprep
method(export_metaboprep, list(Metaboprep, class_character)) <- function(metaboprep, directory, ...) {
  
  # make the directories
  message("Exporting in metaboprep format to ", directory)
  today    <- gsub("-", "_", Sys.Date())
  dir      <- file.path(sub("/$", "", directory), paste0("metaboprep_export_", today))
  layers   <- stats::setNames(dimnames(metaboprep@data)[[3]], dimnames(metaboprep@data)[[3]])
  sub_dirs <- lapply(layers, function(x) file.path(dir, x))
  invisible(lapply(sub_dirs, dir.create, showWarnings = FALSE, recursive = TRUE))

  
  # combine and write the data for each layer
  for (j in seq_along(layers)) {

    # this data layer
    layer <- layers[[j]]

    
    # deal with feature tree separately
    tree      <- attr(metaboprep@feature_summary, paste0(layer, "_tree"))
    tree_path <- NULL
    if (!is.null(tree)) {
      tree_path <- file.path(sub_dirs[[layer]], "feature_tree.RDS")
      saveRDS(tree, tree_path)
    }

    
    # gather the yaml config parameters
    config <- list(layer_index = j,
                   layer_name  = layer,
                   sample_missingness        = attr(metaboprep@data, paste0(layer, "_sample_missingness")), 
                   feature_missingness       = attr(metaboprep@data, paste0(layer, "_feature_missingness")), 
                   total_peak_area_sd        = attr(metaboprep@data, paste0(layer, "_total_peak_area_sd")), 
                   outlier_udist             = attr(metaboprep@data, paste0(layer, "_outlier_udist")), 
                   outlier_treatment         = attr(metaboprep@data, paste0(layer, "_outlier_treatment")), 
                   winsorize_quantile        = attr(metaboprep@data, paste0(layer, "_winsorize_quantile")), 
                   tree_cut_height           = attr(metaboprep@data, paste0(layer, "_tree_cut_height")), 
                   pc_outlier_sd             = attr(metaboprep@data, paste0(layer, "_pc_outlier_sd")), 
                   features_exclude_but_keep = attr(metaboprep@data, paste0(layer, "_features_exclude_but_keep")))

    
    # exclusions
    excl_feats <- ifelse(layer=="qc", unlist(metaboprep@exclusions[["features"]]), character(0L))
    excl_samps <- ifelse(layer=="qc", unlist(metaboprep@exclusions[["samples"]]),  character(0L))
    
    
    # update config
    config[["total_n_features"]]    <- ncol(metaboprep@data)
    config[["total_n_samples"]]     <- nrow(metaboprep@data)
    config[["included_n_features"]] <- length(setdiff(colnames(metaboprep@data), excl_feats))
    config[["included_n_samples"]]  <- length(setdiff(rownames(metaboprep@data), excl_samps))
    config[["excluded_features"]]   <- excl_feats
    config[["excluded_samples"]]    <- excl_samps

    
    # get the features
    features   <- metaboprep@features
    incl_feats <- features[!features$feature_id %in% excl_feats, "feature_id"]

    
    # get the samples
    samples    <- metaboprep@samples
    incl_samps <- samples[!samples$sample_id %in% excl_samps, "sample_id"]
    
    
    # get the data
    data <- metaboprep@data[incl_samps, incl_feats, layer]
    data <- cbind(
      data.frame("sample_id" = rownames(data)),
      as.data.frame(data)
    )

    
    # write the samples & features
    write.table(samples[samples$sample_id %in% incl_samps, ], file.path(sub_dirs[[layer]], "samples.tsv"), sep="\t", row.names = FALSE, quote = FALSE)
    write.table(features[features$feature_id %in% incl_feats, ], file.path(sub_dirs[[layer]], "features.tsv"), sep="\t", row.names = FALSE, quote = FALSE)
    write.table(data, file.path(sub_dirs[[layer]], "data.tsv"), sep="\t", row.names = FALSE, quote = FALSE)
    
    
    # write the feature summary if present
    if (layer %in% dimnames(metaboprep@feature_summary)[[3]]) {
      feat_sum <- t(metaboprep@feature_summary[, incl_feats, layer])
      feat_sum <- cbind(
        data.frame("feature_id" = rownames(feat_sum)),
        as.data.frame(feat_sum)
      )
      write.table(feat_sum, file.path(sub_dirs[[layer]], "feature_summary.tsv"), sep="\t", row.names = FALSE, quote = FALSE)
      
      # add tree path to config 
      config[["tree_path"]]        <- tree_path
    }

    
    # write the sample summary if present
    if (layer %in% dimnames(metaboprep@sample_summary)[[3]]) {
      samp_sum <- metaboprep@sample_summary[incl_samps, , layer]
      samp_sum <- cbind(
        data.frame("sample_id" = rownames(samp_sum)),
        as.data.frame(samp_sum)
      )
      write.table(samp_sum, file.path(sub_dirs[[layer]], "sample_summary.tsv"), sep="\t", row.names = FALSE, quote = FALSE)
      
      # write variance explained
      var_exp <- attr(metaboprep@sample_summary, paste0(layer, "_varexp"))
      if (!is.null(var_exp)) {
        ve <- data.frame("pc"    = names(var_exp),
                         "value" = var_exp)
        write.table(ve, file.path(sub_dirs[[layer]], "var_exp.tsv"), sep="\t", row.names = FALSE, quote = FALSE)
      }
      
      # write analysis data
      config[["num_pcs_scree"]]    <- attr(metaboprep@sample_summary, paste0(layer, "_num_pcs_scree"))
      config[["num_pcs_parallel"]] <- attr(metaboprep@sample_summary, paste0(layer, "_num_pcs_parallel"))
    }

    # write config
    yaml::write_yaml(config, file.path(sub_dirs[[layer]], "config.yml"))
  }

  invisible(metaboprep)
}


#' @title Export Data to `COMETS` format
#' @inheritParams export
#' @param layer character, the name of the `metaboprep@data` layer (3rd array dimension) to write out 
#' @return the `Metaboprep` object, invisibly, for use in pipes
#' @importFrom openxlsx addWorksheet writeData saveWorkbook
#' @export
export_comets <- new_generic("export_comets", c("metaboprep", "directory"), function(metaboprep, directory, layer = NULL) { S7_dispatch() })
#' @name export_comets
method(export_comets, list(Metaboprep, class_character)) <- function(metaboprep, directory, layer = NULL) {

  # testing 
  if (FALSE) {
    library(devtools)
    load_all()
    directory <- "~/Desktop/metaboprep_test"
    data     <- read.csv(system.file("extdata", "dummy_data.csv",     package = "metaboprep"), header=T, row.names = 1) |> as.matrix()
    samples  <- read.csv(system.file("extdata", "dummy_samples.csv",  package = "metaboprep"), header=T, row.names = 1)
    features <- read.csv(system.file("extdata", "dummy_features.csv", package = "metaboprep"), header=T, row.names = 1)
    metaboprep <- Metaboprep(data = data, samples = samples, features = features)
  }
  
  
  # init ====
  today  <- gsub("-", "_", Sys.Date())
  fp     <- file.path(sub("/$", "", directory), paste0("metaboprep_comets_export_", today, ".xlsx"))
  dir.create(dirname(fp), showWarnings = FALSE)
  layers <- dimnames(metaboprep@data)[[3]]
  if (is.null(layer)) {
    if ("qc" %in% layers) {
      layer <- "qc"
    } else if ("input" %in% layers) {
      layer <- "input"
    } else {
      warning("Neither 'qc' nor 'input' layer found in metaboprep@data.")
      layer <- NULL
    }
  } else {
    if (!(layer %in% layers)) {
      stop(sprintf("Specified layer '%s' not found in metaboprep@data.", layer))
    }
  }
  message(paste0("Exporting data layer `", layer, "` in comets format to ", fp))
    
  
  # extract data ====
  data     <- metaboprep@data[, , layer] |> as.data.frame()
  data     <- cbind(SAMPLE_ID = rownames(data), data)
  samples  <- metaboprep@samples
  features <- metaboprep@features
  names(samples)[names(samples) == "sample_id"] <- "SAMPLE_ID"
  names(features)[names(features) == "feature_id"] <- "metabid"
  name_cols <- grep("name", names(features), ignore.case = TRUE, value = TRUE)
  if (length(name_cols) > 0) {
    names(features)[names(features) == name_cols[1]] <- "metabolite_name"
  } else {
    features$metabolite_name <- features$metabid
  }
  
  
  # create other sheets ====
  guess_vartype_and_accepted <- function(column) {
    unique_vals <- sort(unique(column))
    n_unique <- length(unique_vals)
    
    # Check if numeric
    if (is.numeric(column)) {
      # Check if continuous or categorical numeric
      if (all(column %% 1 == 0)) {  # all integers
        if (n_unique <= 10) {
          # categorical integer values
          vartype <- "categorical"
          accepted <- paste(unique_vals, collapse = ", ")
        } else {
          # large number of unique integers -> treat as continuous
          vartype <- "continuous"
          if (min(column) >= 0) {
            accepted <- "[0, Inf)"
          } else {
            accepted <- "(-Inf, Inf)"
          }
        }
      } else {
        # numeric with decimals = continuous
        vartype <- "continuous"
        if (min(column) >= 0) {
          accepted <- "[0, Inf)"
        } else {
          accepted <- "(-Inf, Inf)"
        }
      }
    } else if (is.factor(column) || is.character(column)) {
      # categorical for factors and characters
      vartype <- "categorical"
      if (n_unique <= 10) {
        accepted <- paste(unique_vals, collapse = ", ")
      } else {
        accepted <- NA_character_
      }
    } else {
      vartype <- NA_character_
      accepted <- NA_character_
    }
    
    return(c(vartype, accepted))
  }
  
  # Apply function to your sample dataframe
  results <- t(sapply(samples, guess_vartype_and_accepted))
  colnames(results) <- c("VARTYPE", "ACCEPTED_VALUES")
  results["SAMPLE_ID", "VARTYPE"] <- "NA"
  results["SAMPLE_ID", "ACCEPTED_VALUES"] <- NA_character_
  
  var_map <- data.frame(
    VARREFERENCE = names(samples),
    VARDEFINITION = paste("Definition for", names(samples)),
    COHORTVARIABLE = names(samples),
    VARTYPE = results[, "VARTYPE"],
    ACCEPTED_VALUES = results[, "ACCEPTED_VALUES"],
    COHORTNOTES = paste("Notes for", names(samples)),
    stringsAsFactors = FALSE
  )
  var_map <- rbind(
    data.frame(
      VARREFERENCE = "metabolite_id",
      VARDEFINITION = "Definition for metabid",
      COHORTVARIABLE = "metabid",
      VARTYPE = "NA",
      ACCEPTED_VALUES = NA_character_,
      COHORTNOTES = "Notes for metabid",
      stringsAsFactors = FALSE
    ),
    var_map
  )
  var_map[which(var_map$VARREFERENCE == "SAMPLE_ID"), "VARREFERENCE"] <- "id"
  
  # model dataframe
  exposure_vars <- setdiff(names(samples), "SAMPLE_ID")
  exposure_reference <- rep(NA_character_, length(exposure_vars))
  for (i in seq_along(exposure_vars)) {
    col <- samples[[exposure_vars[i]]]
    if (is.factor(col)) {
      exposure_reference[i] <- as.character(levels(col)[1])  # first factor level
    } else if (is.character(col)) {
      exposure_reference[i] <- sort(unique(col))[1]  # first unique string value
    }
  }
  models <- data.frame(
    MODEL = paste(1:length(exposure_vars), exposure_vars),
    OUTCOMES = "All metabolites",
    EXPOSURE = exposure_vars,
    EXPOSURE_REFERENCE = exposure_reference,
    ADJUSTMENT = NA_character_,
    STRATIFICATION = NA_character_,
    WHERE = NA_character_,
    TIME = NA_character_,
    GROUP = NA_character_,
    MODEL_TYPE = "corr (spearman)",
    stringsAsFactors = FALSE
  )
  
  # write to excel ====
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "Metabolites")
  openxlsx::addWorksheet(wb, "SubjectMetabolites")
  openxlsx::addWorksheet(wb, "SubjectData")
  openxlsx::addWorksheet(wb, "VarMap")
  openxlsx::addWorksheet(wb, "Models")
  
  openxlsx::writeData(wb, "Metabolites", features)
  openxlsx::writeData(wb, "SubjectMetabolites", data)
  openxlsx::writeData(wb, "SubjectData", samples)
  openxlsx::writeData(wb, "VarMap", var_map)
  openxlsx::writeData(wb, "Models", models)
  
  openxlsx::saveWorkbook(wb, fp, overwrite = TRUE)
  
  message("Export complete.")
  invisible(fp)
}



#' @title Export Data to `MetaboAnalyst` format
#' @inheritParams export
#' @param something something
#' @return the `Metaboprep` object, invisibly, for use in pipes
#'
#' @export
export_metaboanalyst <- new_generic("export_metaboanalyst", c("metaboprep", "directory"), function(metaboprep, directory, something="foo") { S7_dispatch() })
#' @name export_metaboanalyst
method(export_metaboanalyst, list(Metaboprep, class_character)) <- function(metaboprep, directory, something="foo") {
  message("Exporting in export_metaboanalyst format to ", directory)
  # code

  
  
  
  
}



