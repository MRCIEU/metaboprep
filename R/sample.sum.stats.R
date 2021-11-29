#' summary statistics for samples
#'
#' This function estimates summary statistics for samples in a matrix of numeric features. This includes missingness, total peak area, and a count of the number of outlying features for a sample.
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param feature_names_2_exclude a vector of feature|column names to exclude from missingness estimates
#' @param outlier_udist the interquartile range unit distance from the median to call a sample an outlier at a feature.
#'
#' @keywords sample summary statistics
#' 
#' @return a data frame of summary statistics
#'
#' @export
#'
#' @examples
#' ## simulate some data
#' set.seed(1110)
#' ex_data = sapply(1:5, function(x){ rnorm(10, 40, 5) })
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## add some missingness to the data
#' ex_data[ sample(1:50, 10) ] = NA
#' ## run estimate sample summary statistics
#' sample.sum.stats(ex_data)
#'
sample.sum.stats = function( wdata, feature_names_2_exclude = NA, outlier_udist = 5){
  ## missingness
  if( is.na(feature_names_2_exclude[1]) == TRUE ){
    samplemis = sample.missingness(wdata)
  } else {
    samplemis = sample.missingness(wdata, feature_names_2_exclude)
  }
  ### total peak area
  tpa = total.peak.area(wdata)
  ### sample outliers
  sout = sample.outliers(wdata, nsd = outlier_udist)
  ### data out
  out = cbind(samplemis, tpa, sout )
  ###
  sample_id = rownames(out)
  out = cbind(sample_id, out)
  ##
  return(out)
  }




