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
                              validator = function(value) if (length(setdiff(c("feature_id"), names(value))) > 0) "should have at least the column named `feature_id`" else NULL),
    # a list of exclusions
    exclusions = new_property(class     = class_list,
                              validator = function(value) {
                                if (length(value)==0) return(NULL)
                                check_samp  <- identical(names(value[["samples"]]), c("user_excluded", "extreme_sample_missingness","user_defined_sample_missingness", "user_defined_sample_totalpeakarea", "user_defined_sample_pca_outlier"))
                                check_feat  <- identical(names(value[["features"]]), c("user_excluded", "extreme_feature_missingness","user_defined_feature_missingness"))
                                if (check_samp && check_feat) {
                                  return(NULL)
                                } else {
                                  return("should be a lists with names 'samples' and 'features'. In addition, 'samples' should be a list of character vectors named 'user_excluded', extreme_sample_missingness','user_defined_sample_missingness', 'user_defined_sample_totalpeakarea', 'user_defined_sample_pca_outlier'; and 'features' a list of character vectors named 'user_excluded', 'extreme_feature_missingness', 'user_defined_feature_missingness'")
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
  constructor = function(data, samples, features, 
                         exclusions = list(samples  = list(user_excluded                     = character(),
                                                           extreme_sample_missingness        = character(),
                                                           user_defined_sample_missingness   = character(),
                                                           user_defined_sample_totalpeakarea = character(),
                                                           user_defined_sample_pca_outlier   = character()),
                                           features = list(user_excluded                     = character(),
                                                           extreme_feature_missingness       = character(),
                                                           user_defined_feature_missingness  = character())), 
                         feature_summary = array(data = NA_real_, dim = c(0,0,0)), 
                         sample_summary  = array(data = NA_real_, dim = c(0,0,0))) {
    # if data is a matrix, convert to 3D array
    if (is.matrix(data)) {
      data <- array(data, dim = c(nrow(data), ncol(data), 1), dimnames = list(rownames(data), colnames(data), "input"))
    }
    
    # check all sample and feature ids in data
    stopifnot("data matrix must be named with sample_ids and feature_ids" = all(rownames(data) %in% samples[["sample_id"]] & all(colnames(data) %in% features[["feature_id"]])))
    
    # order samples and features by data
    samples  <- as.data.frame(samples[match(rownames(data), samples[["sample_id"]]), , drop = FALSE])
    features <- as.data.frame(features[match(colnames(data), features[["feature_id"]]), , drop = FALSE])
    
    # populate object and return it
    new_object(.parent         = S7_object(), 
               data            = data, 
               samples         = samples, 
               features        = features, 
               exclusions      = exclusions,
               sample_summary  = sample_summary, 
               feature_summary = feature_summary)
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


#' @title Add a Layer of Data (internal use)
#' @description
#' This function adds an additional layer of data along the third dimension to an existing 3D array (or 2D matrix/vector) by stacking a new layer of data.
#' It ensures that the dimensions of the new layer match the first two dimensions of the existing array or matrix.
#' If there is a mismatch in row or column names and the `force` parameter is set to `TRUE`, the function will align the data by filling missing values with `NA`.
#' It is used internally and not intended for routine user use.
#'
#' @param current A vector, matrix, or 3D array representing the current stack of data.
#' @param layer A matrix or array that represents the new layer of data to be added. It should match the dimensions of the first two dimensions of `current`.
#' @param layer_name A character string specifying the name of the new dimension for the 3rd axis. This can be used to annotate the new data layer.
#' @param force A logical value indicating whether to force the join and create `NA` values where row or column names do not match between `current` and `layer`. Default is `FALSE`.
#'
#' @returns A 3D array with the added layer in the third dimension.
#'
#' @export
add_layer <- function(current, layer, layer_name, force=FALSE) {
  
  if (is.vector(layer)) {
    layer <- array(layer,
                   dim = c(length(layer), 1, 1),
                   dimnames = list(names(layer), "value", layer_name))
  } else if (is.matrix(layer)) {
    layer <- array(layer,
                   dim = c(nrow(layer), ncol(layer), 1),
                   dimnames = list(rownames(layer), colnames(layer), layer_name))
  }
  
  # If current is empty, initialize it directly with the first layer (no NAs)
  if (length(current) == 0) {
    return(layer)
  }
  
  # Ensure row and column names match before appending
  if (!identical(rownames(current), rownames(layer)) ||
      !identical(colnames(current), colnames(layer))) {
    
    # force together anyway (ok for things like PCs or varexp)
    if (force) {
      all_rows <- union(rownames(current), rownames(layer))
      all_cols <- union(colnames(current), colnames(layer))
      
      current_expanded <- array(NA_real_,
                                dim = c(length(all_rows), length(all_cols), dim(current)[3]),
                                dimnames = list(all_rows, all_cols, dimnames(current)[[3]]))
      current_expanded[rownames(current), colnames(current), ] <- current
      
      # keep attributes
      current_attrs <- attributes(current)
      for (att in setdiff(names(current_attrs), c("dim", "dimnames"))) {
        attr(current_expanded, att) <- current_attrs[[att]]
      }
      
      layer_expanded <- array(NA_real_,
                              dim = c(length(all_rows), length(all_cols), 1),
                              dimnames = list(all_rows, all_cols, layer_name))
      layer_expanded[rownames(layer), colnames(layer), ] <- layer
      current <- current_expanded
      layer <- layer_expanded
      
    } else {
      stop("Error: Row names and column names must match before joining layers.")
    }
  }
  
  # Check if the layer_name already exists in the current array depth
  existing_layer_index <- which(dimnames(current)[[3]] == layer_name)
  
  # If the layer exists, overwrite it
  if (length(existing_layer_index) > 0) {
    
    current[, , existing_layer_index] <- layer
    
    # If it doesnt append layer
  } else {
    
    if (is.array(current)) {
      
      current <- array(c(current, layer),
                       dim = c(dim(layer)[1], dim(layer)[2], dim(current)[3] + 1),
                       dimnames = list(rownames(layer),
                                       colnames(layer),
                                       c(dimnames(current)[[3]], layer_name)))
      
    } else {
      stop("why is current not an array")
    }
    
  }
  
  return(current)
}