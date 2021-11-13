#' summary statistics for samples
#'
#' This function estimates summary statistics for samples in a matrix of numeric features. This includes missingness, total peak area, and a count of the number of outlying features for a sample.
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param feature_names_2_exclude a vector of feature|column names to exclude from missingness estimates
#'
#' @keywords sample summary statistics
#' 
#' @return a data frame of summary statistics
#'
#' @export
#'
#' @examples
#' sample.sum.stats()
#'
sample.sum.stats = function( wdata, feature_names_2_exclude = NA){
  ## missingness
  if( is.na(feature_names_2_exclude[1]) == TRUE ){
    samplemis = sample.missingness(wdata)
  } else {
    samplemis = sample.missingness(wdata, feature_names_2_exclude)
  }
  ### total peak area
  tpa = total.peak.area(wdata)
  ### sample outliers
  sout = sample.outliers(wdata)
  ### data out
  out = cbind(samplemis, tpa, sout )
  ###
  sample_id = rownames(out)
  out = cbind(sample_id, out)
  ##
  return(out)
  }




