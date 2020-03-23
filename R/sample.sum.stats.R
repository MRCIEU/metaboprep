#' A Function to estimate numerous summary statistics for your samples
#'
#' This function estiamtes summary statistics for samples in a matrix of metabolite features. This includes missingness, total peak area, a count of the number of outlying features for a sample.
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param features_names_2_exclude a vector of feature|column names to exclude from missingness estimates
#' @keywords metabolomics
#' @export
#' @examples
#' sample.sum.stats()
sample.sum.stats = function( wdata, features_names_2_exclude = NA){
  ## missingness
  if( is.na(features_names_2_exclude[1]) == TRUE ){
    samplemis = sample.missingness(wdata)
  } else {
    samplemis = sample.missingness(wdata, features_names_2_exclude)
  }
  ### total peak area
  tpa = total.peak.area(wdata)
  ### sample outliers
  sout = sample.outliers(wdata)
  ### data out
  out = cbind(samplemis, tpa, sout )
  return(out)
  }




