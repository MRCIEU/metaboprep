#' @title Metabolite Quality Control
#' @description
#' This function is a wrapper function that performs the key quality controls steps on a metabolomics data set.
#' Key principles:
#'  1. keep the source underlying data as it is
#'  2. copy the source data to a new data layer called qcing for processing
#'  3. build an exclusion list, accumulating codes for exclusion reasons
#'  4. make any adjustments needed in the destination copy of the data, flag these in the exclusion list
#'  5. copy the final result to a data layer called post_qc
#'  6. return the Metabolites object with the newly populated data layers
#'
#' @inheritParams feature_summary
#' @inheritParams sample_summary
#' @param sample_missingness numeric 0-1, percentage of data missingness which should prompt exclusion of a sample
#' @param feature_missingness numeric 0-1, percentage of data missingness which should prompt exclusion of a feature
#' @param total_peak_area_sd numeric, number of TPA SD after which a sample would be excluded
#' @param outlier_treatment character, how to handle outlier data values - options 'leave_be', 'turn_NA', or 'winsorize'
#' @param winsorize_quantile numeric, quantile to winsorize to, only relevant if 'outlier_treatment'='winsorize'
#' @param pc_outlier_sd numeric, number of PCA SD after which a sample would be excluded
#' 
#' @include class_metaboprep.R
#' @importFrom stats quantile
#' @importFrom glue glue
#' @import cli
#' 
#' @export
quality_control <- new_generic("quality_control", c("metaboprep"), function(metaboprep, source_layer="input", sample_missingness = 0.5, feature_missingness = 0.5, total_peak_area_sd = 5, outlier_udist=5, outlier_treatment="leave_be", winsorize_quantile = 1.0, tree_cut_height=0.5, pc_outlier_sd =5, sample_ids=NULL, feature_ids=NULL, features_exclude_but_keep=NULL) { S7_dispatch() })
#' @name quality_control
method(quality_control, Metaboprep) <- function(metaboprep, source_layer="input", sample_missingness = 0.5, feature_missingness = 0.5, total_peak_area_sd = 5, outlier_udist=5, outlier_treatment="leave_be", winsorize_quantile = 1.0, tree_cut_height=0.5, pc_outlier_sd =5, sample_ids=NULL, feature_ids=NULL, features_exclude_but_keep=NULL){

  cli::cli_h1("Starting Metabolite QC Process")


  # input validation
  cli::cli_alert_info("Validating input parameters...")
  source_layer <- match.arg(source_layer, choices = dimnames(metaboprep@data)[[3]])
  outlier_treatment <- match.arg(outlier_treatment, choices = c("leave_be", "turn_NA", "winsorize"))
  stopifnot("sample_ids must all be found in the data" = is.null(sample_ids) || all(sample_ids %in% metaboprep@samples[["sample_id"]]))
  stopifnot("feature_ids must all be found in the data" = is.null(feature_ids) || all(feature_ids %in% metaboprep@features[["feature_id"]]))  
  stopifnot("`features_exclude_but_keep` must be a logical column in the features data or a vector of feature ids all present in the data" = is.null(features_exclude_but_keep) || (features_exclude_but_keep %in% names(metaboprep@features) && all(is.logical(metaboprep@features[[features_exclude_but_keep]]))) || all(features_exclude_but_keep %in% metaboprep@features[["feature_id"]]) )
  cli::cli_alert_success("Validating input parameters...")


  # get ids & update exclusions if user has passed a predefined set of ids
  if (is.null(sample_ids)) {
    sample_ids   <- metaboprep@samples[["sample_id"]] 
  } else {
    cli::cli_alert_info(glue::glue("Assessing sample_id input"))
    excl_samps <- setdiff(metaboprep@samples[["sample_id"]], sample_ids)
    metaboprep@exclusions$samples$user_excluded <- excl_samps
    cli::cli_alert_success(glue::glue("Provided samples results in exclusion of {length(excl_samps)} sample(s)."))
  }
  if (is.null(feature_ids)) {
    feature_ids <- metaboprep@features[["feature_id"]]
  } else {
    cli::cli_alert_info(glue::glue("Assessing feature_id input"))
    excl_feats <- setdiff(metaboprep@features[["feature_id"]], feature_ids)
    metaboprep@exclusions$features$user_excluded <- excl_feats
    cli::cli_alert_success(glue::glue("Provided samples results in exclusion of {length(excl_feats)} feature(s)."))
  }


  # exclude from analysis but keep in data features
  exclude_but_keep_feats <- character()
  if (!is.null(features_exclude_but_keep)) {
    if (length(features_exclude_but_keep)==1 && features_exclude_but_keep %in% colnames(metaboprep@features)) {
      exclude_but_keep_feats <- metaboprep@features[metaboprep@features[[features_exclude_but_keep]]==TRUE, "feature_id"]
      cli::cli_alert_info(glue::glue("Excluding {length(exclude_but_keep_feats)} features from sample summary analysis but keeping in output data, flagged form column `{features_exclude_but_keep}`..."))
    } else {
      exclude_but_keep_feats <- features_exclude_but_keep
      cli::cli_alert_info(glue::glue("Excluding {length(exclude_but_keep_feats)} features from sample summary analysis but keeping in output data, identified from `features_exclude_but_keep` argument..."))
    }
  } 
  

  # run sample summary and feature summary on the raw data
  cli::cli_alert_info(glue::glue("Sample & Feature Summary Statistics for raw data..."))
  stopifnot("No remaining features" = length(setdiff(feature_ids, exclude_but_keep_feats)) > 0)
  stopifnot("No remaining samples"  = length(sample_ids) > 0)
  metaboprep <- summarise(metaboprep, 
                          source_layer     = source_layer, 
                          outlier_udist    = outlier_udist, 
                          tree_cut_height  = tree_cut_height, 
                          sample_ids       = sample_ids, 
                          feature_ids      = feature_ids,
                          features_exclude = exclude_but_keep_feats,
                          output           = "object")
  cli::cli_alert_success(glue::glue("Sample & Feature Summary Statistics for raw data..."))


  # data to work from and add another layer - we now work with the 'destination data', plus any exclusions, from now on
  cli::cli_alert_info(glue::glue("Copying {source_layer} data to new 'qc' data layer..."))
  dat <- metaboprep@data[, , source_layer]
  dat[!rownames(dat) %in% sample_ids, ]  <- NA_real_
  dat[, !colnames(dat) %in% feature_ids] <- NA_real_
  metaboprep@data <- add_layer(current    = metaboprep@data,
                               layer      = dat,
                               layer_name = "qc")
  cli::cli_alert_success(glue::glue("data ready for QC."))


  # very bad sample missingness
  cli::cli_alert_info(glue::glue("Assessing for extreme sample missingness >=80%..."))
  est_samps  <- sample_ids
  est_feats  <- setdiff(feature_ids, exclude_but_keep_feats)
  stopifnot("No remaining features" = length(est_feats) > 0)
  stopifnot("No remaining samples" = length(est_samps) > 0)
  dat        <- metaboprep@data[est_samps, est_feats, "qc"]
  samplemis  <- missingness(dat, by="row")
  excl_samps <- samplemis[samplemis$missingness >= 0.8, "sample_id"]
  metaboprep@exclusions$samples$extreme_sample_missingness <- excl_samps
  sample_ids <- setdiff(sample_ids, excl_samps)
  cli::cli_alert_success(glue::glue("Extreme sample missingness exclusions assessment complete. ({length(excl_samps)} samples excluded)"))

  
  # very bad feature missingness
  cli::cli_alert_info(glue::glue("Assessing for extreme feature missingness >=80%..."))
  est_samps  <- sample_ids
  est_feats  <- setdiff(feature_ids, exclude_but_keep_feats)
  stopifnot("No remaining features" = length(est_feats) > 0)
  stopifnot("No remaining samples" = length(est_samps) > 0)
  dat        <- metaboprep@data[est_samps, est_feats, "qc"]
  featuremis <- missingness(dat, by="column")
  excl_feats <- featuremis[featuremis$missingness >= 0.8, "feature_id"]
  metaboprep@exclusions$features$extreme_feature_missingness <- unique(metaboprep@exclusions$features$extreme_feature_missingness, excl_feats)
  feature_ids <- setdiff(feature_ids, excl_feats)
  cli::cli_alert_success(glue::glue("Extreme feature missingness exclusions assessment complete. ({length(excl_feats)} features excluded)"))


  # re-estimate sample missingness and exclude based on user-defined
  if (!is.null(sample_missingness) && !is.na(sample_missingness)) {
    cli::cli_alert_info(glue::glue("Assessing for sample missingness at specified level of >={round(sample_missingness*100)}%..."))
    est_samps  <- sample_ids
    est_feats  <- setdiff(feature_ids, exclude_but_keep_feats)
    stopifnot("No remaining features" = length(est_feats) > 0)
    stopifnot("No remaining samples" = length(est_samps) > 0)
    dat        <- metaboprep@data[est_samps, est_feats, "qc"]
    samplemis  <- missingness(dat, by="row")
    excl_samps <- samplemis[samplemis$missingness >= sample_missingness, "sample_id"]
    metaboprep@exclusions$samples$user_defined_sample_missingness <- excl_samps
    sample_ids <- setdiff(sample_ids, excl_samps)
    cli::cli_alert_success(glue::glue("Sample missingness exclusions assessment complete. ({length(excl_samps)} samples excluded)"))
  } else {
    cli::cli_alert_warning(glue::glue("Skipping assessment of sample missingness from user defined threshold [sample_missingness={sample_missingness}]..."))
  }

  
  # re-estimate feature missingness
  if (!is.null(feature_missingness) && !is.na(feature_missingness)) {
    cli::cli_alert_info(glue::glue("Assessing for feature missingness at specified level of >={round(feature_missingness*100)}%..."))
    est_samps  <- sample_ids
    est_feats  <- setdiff(feature_ids, exclude_but_keep_feats)
    stopifnot("No remaining features" = length(est_feats) > 0)
    stopifnot("No remaining samples" = length(est_samps) > 0)
    dat        <- metaboprep@data[sample_ids, est_feats, "qc"]
    featuremis <- missingness(dat, by="column")
    excl_feats <- featuremis[featuremis$missingness >= feature_missingness, "feature_id"]
    metaboprep@exclusions$features$user_defined_feature_missingness <- excl_feats
    feature_ids <- setdiff(feature_ids, excl_feats)
    cli::cli_alert_success(glue::glue("Feature missingness exclusions assessment complete. ({length(excl_feats)} features excluded)"))
  } else {
    cli::cli_alert_warning(glue::glue("Skipping assessment of feature missingness from user defined threshold [feature_missingness={feature_missingness}]..."))
  }

  
  # total peak area
  if (!is.null(total_peak_area_sd) && !is.na(total_peak_area_sd)) {
    cli::cli_alert_info(glue::glue("Calculating total peak abundance outliers at +/- {total_peak_area_sd} Sdev..."))
    est_samps     <- sample_ids
    est_feats     <- setdiff(feature_ids, exclude_but_keep_feats)
    stopifnot("No remaining features" = length(est_feats) > 0)
    stopifnot("No remaining samples" = length(est_samps) > 0)
    dat           <- metaboprep@data[est_samps, est_feats, "qc"]
    tpa           <- total_peak_area(dat)
    tpa[["sdev"]] <- sd(tpa$tpa_total, na.rm = TRUE)
    tpa[["mean"]] <- mean(tpa$tpa_total, na.rm = TRUE)
    tpa[["UL"]]   <- tpa$mean + tpa$sdev * total_peak_area_sd
    tpa[["LL"]]   <- tpa$mean - tpa$sdev * total_peak_area_sd
    excl_samps    <- tpa$sample_id[!(tpa$tpa_total >= tpa$LL & tpa$tpa_total <= tpa$UL)]
    metaboprep@exclusions$samples$user_defined_sample_totalpeakarea <- excl_samps
    sample_ids    <- setdiff(sample_ids, excl_samps)
    cli::cli_alert_success(glue::glue("Total peak abundance outlier exclusions assessment complete. ({length(excl_samps)} samples excluded)"))
  } else {
    cli::cli_alert_warning(glue::glue("Skipping assessment of outliers based on total peak abundance [total_peak_area_sd={total_peak_area_sd}]..."))
  }


  # Sample PCA analysis - first deal with outlier data depending on option selected
  if (!is.null(outlier_udist) && !is.na(outlier_udist)) {
    cli::cli_alert_info(glue::glue("Running sample data PCA outlier analysis at +/- {outlier_udist} Sdev..."))
    est_samps     <- sample_ids
    est_feats     <- setdiff(feature_ids, exclude_but_keep_feats)
    stopifnot("No remaining features" = length(est_feats) > 0)
    stopifnot("No remaining samples" = length(est_samps) > 0)
    dat           <- metaboprep@data[est_samps, est_feats, "qc"]
    if (outlier_treatment != "leave_be") {
  
      omat <- outlier_detection(dat, nsd = outlier_udist, meansd = FALSE, by="column")
  
      # which samples and features to set NA
      indices      <- which(omat == 1, arr.ind = TRUE)
      adjust_samps <- rownames(omat)[indices[,1]]
      adjust_feats <- colnames(omat)[indices[,2]]
  
      if(metabolites@outlier_treatment == "turn_NA") {
  
        # turn NA in actual destination data
        metaboprep@data[adjust_samps, adjust_feats, "qc"] <- NA_real_
        cli::cli_alert_info(glue::glue("All identified outliers were turned into NA..."))
  
      } else if (outlier_treatment == "winsorize") {
  
        for(fid in adjust_feats) {
  
          # quantile value of the feature, minus the outliers
          quantile_value <- quantile(dat[!rownames(dat) %in% adjust_samps, fid], probs = c(winsorize_quantile), na.rm = TRUE)
  
          # set in the destination data
          metabolites@data[adjust_samps, fid, "qc"] <- quantile_value
          cli::cli_alert_info(glue::glue("Outliers were winsorized to the {winsorize_quantile * 100} quantile of remaining (non outlying) values."))
        }
      }
    }#end dealing with adjustments pre PCA run
    cli::cli_alert_info(glue::glue("Sample data outlier analysis complete."))
  } else {
    cli::cli_alert_warning(glue::glue("Skipping assessment & adjustment of outliers based on faw data values [outlier_udist={outlier_udist}]..."))
  }

  
  # perform exclusion on top PCs to ID outliers
  if (!is.null(pc_outlier_sd) && !is.na(pc_outlier_sd)) {
    cli::cli_alert_info(glue::glue("Sample PCA outlier analysis - re-identify feature independence and PC outliers..."))
    metaboprep  <- summarise(metaboprep,  
                             source_layer     = "qc", 
                             outlier_udist    = outlier_udist, 
                             tree_cut_height  = tree_cut_height, 
                             sample_ids       = sample_ids, 
                             feature_ids      = feature_ids,
                             features_exclude = exclude_but_keep_feats,
                             output           = "object")

    pca_data   <- metaboprep@sample_summary[sample_ids, grep("^pc[0-9]+$", colnames(metaboprep@sample_summary), value=TRUE), "qc"]
    outliers   <- outlier_detection(pca_data, nsd = pc_outlier_sd, meansd = TRUE, by = "column")
    excl_samps <- names(which(apply(outliers, 1, function(x) sum(x) > 0)))
    metaboprep@exclusions$samples$user_defined_sample_pca_outlier <- excl_samps
    sample_ids <- setdiff(sample_ids, excl_samps)
    cli::cli_alert_info(glue::glue("Sample PCA outlier analysis complete."))
  } else {
    cli::cli_alert_warning(glue::glue("Skipping assessment of outliers based on sample PCs [pc_outlier_sd={pc_outlier_sd}]..."))
  }
  

  # Make final QC dataset
  cli::cli_alert_info(glue::glue("Creating final QC dataset..."))
  metaboprep <- summarise(metaboprep, 
                          source_layer     = "qc", 
                          outlier_udist    = outlier_udist, 
                          tree_cut_height  = tree_cut_height, 
                          sample_ids       = sample_ids, 
                          feature_ids      = feature_ids,
                          features_exclude = exclude_but_keep_feats,
                          output           = "object")
  
  
  # set parameters used
  attr(metaboprep@data, "qc_sample_missingness")        <- sample_missingness
  attr(metaboprep@data, "qc_feature_missingness")       <- feature_missingness
  attr(metaboprep@data, "qc_total_peak_area_sd")        <- total_peak_area_sd
  attr(metaboprep@data, "qc_outlier_udist")             <- outlier_udist
  attr(metaboprep@data, "qc_outlier_treatment")         <- outlier_treatment
  attr(metaboprep@data, "qc_winsorize_quantile")        <- winsorize_quantile
  attr(metaboprep@data, "qc_tree_cut_height")           <- tree_cut_height
  attr(metaboprep@data, "qc_pc_outlier_sd")             <- pc_outlier_sd
  attr(metaboprep@data, "qc_features_exclude_but_keep") <- exclude_but_keep_feats

  
  # set exclusion data to NA
  excl_samps <- unique(unlist(metaboprep@exclusions$samples))
  excl_feats <- unique(unlist(metaboprep@exclusions$features))
  metaboprep@data[rownames(metaboprep@data) %in% excl_samps, ,"qc"]  <- NA_real_
  metaboprep@data[, colnames(metaboprep@data) %in% excl_feats, "qc"] <- NA_real_
  
  # flag exclusions and reasons in feature and sample data
  samp_reason_df <- do.call(rbind, lapply(names(metaboprep@exclusions$samples), function(reason) {
    ids <- metaboprep@exclusions$samples[[reason]]
    if (length(ids) == 0) return(data.frame(sample_id = character(), reason_excluded = character()))
    data.frame(sample_id = ids, reason_excluded = reason)
  }))
  if (nrow(samp_reason_df) > 0) {
    samp_reason_df <- aggregate(reason_excluded ~ sample_id, samp_reason_df, FUN = function(x) paste(x, collapse = ";"))
    samp_reason_df$excluded <- TRUE
  } else {
    samp_reason_df$excluded <- logical()
  }
  
  feat_reason_df <- do.call(rbind, lapply(names(metaboprep@exclusions$features), function(reason) {
    ids <- metaboprep@exclusions$features[[reason]]
    if (length(ids) == 0) return(data.frame(feature_id = character(), reason_excluded = character()))
    data.frame(feature_id = ids, reason_excluded = reason)
  }))
  if (nrow(feat_reason_df) > 0) {
    feat_reason_df <- aggregate(reason_excluded ~ feature_id, feat_reason_df, FUN = function(x) paste(x, collapse = ";"))
    feat_reason_df$excluded <- TRUE
  } else {
    feat_reason_df$excluded <- logical()
  }
  
  s <- merge(metaboprep@samples, samp_reason_df, by="sample_id", all = TRUE)
  f <- merge(metaboprep@features, feat_reason_df, by="feature_id", all = TRUE)
  s <- s[order(match(s[["sample_id"]],  rownames(metaboprep@data))), ]
  f <- f[order(match(f[["feature_id"]], colnames(metaboprep@data))), ]
  rownames(s) <- s$sample_id
  rownames(f) <- f$feature_id
  s[is.na(s$excluded), "excluded"] <- FALSE
  f[is.na(f$excluded), "excluded"] <- FALSE
  metaboprep@samples  <- s
  metaboprep@features <- f
  
  cli::cli_alert_success(glue::glue("Sample & Feature Summary Statistics for QC data complete."))

  cli::cli_h1("Metabolite QC Process Completed")

  # return the metabolites with underlying data (+/- adjustments) and exclusion matrix
  return(metaboprep)
}
