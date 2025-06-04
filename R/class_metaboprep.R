# Silence R CMD check
globalVariables(c(""), package = "metaboprep")

#' @title Metaboprep Object
#' @description
#' A `Metaboprep` object is a container for matrices of metabolite data, along with associated metadata.
#' It allows for efficient storage and manipulation of data, supporting quality control, transformations,
#' and various analyses. This object facilitates easy access to data layers, sample and feature summaries,
#' outlier treatment, and more.
#' 
#' @param data numeric matrix, the data matrix containing metabolite values (not to be set directly).
#' @param samples data.frame, a data frame containing sample-related information (not to be set directly).
#' @param features data.frame, a data frame containing feature-related information (not to be set directly).
#' @param exclusions list, holds exclusion codes for data masking (not to be set directly).
#' @param feature_summary numeric matrix, summary statistics for features (not to be set directly).
#' @param sample_summary numeric matrix, summary statistics for samples (not to be set directly).
#'
#' @slot data numeric matrix, the metabolite data.
#' @slot samples data.frame, the samples data frame
#' @slot features data.frame, the features data frame
#' @slot exclusions list, exclusion codes (mask for data).
#' @slot feature_summary numeric matrix, feature summary statistics.
#' @slot sample_summary numeric matrix, sample summary statistics.
#'
#' @return An object of class `Metaboprep` (S7 class).
#'
#' @import S7
#' 
#' @export
#' 
Metaboprep <- new_class(
  name    = "Metaboprep",
  package = "metaboprep",
  ##################################
  # Fields of the Metaboprep object
  ##################################
  properties = list(
    # the data
    data       = new_property(class     = class_numeric,
                              default   = array(NA_real_, dim = c(0,0,0)), 
                              validator = function(value) if (!is.array(value) | !is.numeric(value)) "should be a numeric matrix" else NULL),
    # the sample information
    samples    = new_property(class     = class_data.frame, 
                              default   = data.frame("sample_id"=character()),
                              validator = function(value) if (length(setdiff(c("sample_id"), names(value))) > 0) "should have at least the column named `sample_id`" else NULL),
    # the feature information
    features   = new_property(class     = class_data.frame, 
                              default   = data.frame("feature_id"=character(), "platform"=character(), "pathway"=character(), "derived_feature"=logical()), 
                              validator = function(value) if (length(setdiff(c("feature_id", "platform", "pathway", "derived_feature"), names(value))) > 0) "should have at least columns named feature_id, platform, pathway, derived_feature" else NULL),
    # a list of exclusions
    exclusions = new_property(class     = class_list,
                              validator = function(value) {
                                if (length(value)==0) return(NULL)
                                check_names <- all(sapply(value, function(layer) identical(names(layer), c("samples","features"))))
                                check_samp  <- all(sapply(value, function(layer) identical(names(layer[["samples"]]), c("extreme_sample_missingness","user_defined_sample_missingness", "user_defined_sample_totalpeakarea", "user_defined_sample_pca_outlier"))))
                                check_feat  <- all(sapply(value, function(layer) identical(names(layer[["features"]]), c("extreme_feature_missingness","user_defined_feature_missingness"))))
                                if (check_names && check_samp && check_feat) {
                                  return(NULL)
                                } else {
                                  return("should be a list of lists with names 'samples' and 'features'. In addition, 'samples' should be a list of character vectors named 'extreme_sample_missingness','user_defined_sample_missingness', 'user_defined_sample_totalpeakarea', 'user_defined_sample_pca_outlier'; and 'features' a list of character vectors named 'extreme_feature_missingness', 'user_defined_feature_missingness'")
                                }
                              }),
    # summary data for features
    feature_summary       = new_property(class     = class_numeric,
                                         default   = array(NA_real_, dim = c(0,0,0)),
                                         validator = function(value) if (!is.array(value) | !is.numeric(value)) "should be a numeric matrix" else NULL),
    # summary data for samples
    sample_summary        = new_property(class     = class_numeric,
                                         default   = array(NA_real_, dim = c(0,0,0)),
                                         validator = function(value) if (!is.array(value) | !is.numeric(value)) "should be a numeric matrix" else NULL)
  ),
  ############################################
  # Constructor function the Metaboprep object
  ############################################
  constructor = function(data, samples, features) {
    # if data is a matrix, convert to 3D array
    if (is.matrix(data)) {
      data <- array(data, dim = c(nrow(data), ncol(data), 1), dimnames = list(rownames(data), colnames(data), "input"))
    }
    
    # check all sample and feature ids in data
    stopifnot("data matrix must be named with sample_ids and feature_ids" = all(rownames(data) %in% samples[["sample_id"]] & all(colnames(data) %in% features[["feature_id"]])))
    
    # order samples and features by data
    samples  <- samples[match(rownames(data), samples[["sample_id"]]), , drop = FALSE]
    features <- features[match(colnames(data), features[["feature_id"]]), , drop = FALSE]
    
    # populate an empty exclusions list
    exclusions <- list(raw = list(samples  = list(extreme_sample_missingness        = character(),
                                                  user_defined_sample_missingness   = character(),
                                                  user_defined_sample_totalpeakarea = character(),
                                                  user_defined_sample_pca_outlier   = character()),
                                  features = list(extreme_feature_missingness       = character(),
                                                  user_defined_feature_missingness  = character())))
    
    # populate object and return it
    new_object(.parent         = S7_object(), 
               data            = data, 
               samples         = samples, 
               features        = features, 
               exclusions      = exclusions,
               sample_summary  = array(data = NA_real_, dim = c(0,0,0)), 
               feature_summary = array(data = NA_real_, dim = c(0,0,0)))
  },
  ##########################################
  # Validator function the Metaboprep object
  ##########################################
  validator = function(self) {
    if ((nrow(self@features)>0 & length(self@data)>0) && (nrow(self@features) != ncol(self@data))) {
      sprintf("Number of @features (%i) must equal the number of features in @data (%i)", nrow(self@features), ncol(self@data))
    }
    if ((nrow(self@samples)>0 & length(self@data)>0) && (nrow(self@samples) != nrow(self@data))) {
      sprintf("Number of @samples (%i) must equal the number of samples in @data (%i)", nrow(self@samples), nrow(self@data))
    }
    if ((nrow(self@samples)>0 & length(self@data)>0) && !identical(self@samples[, "sample_id"], rownames(self@data))) {
      "Column `sample_id` in @samples must be identical to the rownames of @data"
    }
    if ((nrow(self@features)>0 & length(self@data)>0) && !identical(self@features[, "feature_id"], colnames(self@data))) {
      "Column `feature_id` in @features must be identical to the colnames of @data"
    }
  }
)
