#' A Function to provide a count of the number outlying samples for a feature
#'
#' This function takes a matrix of metaboite data (samples in rows, features in columns) and counts the number of outlying samples each feature has.
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param nsd the number of standard deviation from the mean outliers are identified at. Default value is 5.
#' @keywords metabolomics
#' @export
#' @examples
#' feature.outliers()
feature.outliers = function(wdata, nsd = 5){
  Omat = outlier.matrix(wdata, nsd)
  o = apply(Omat, 2, sum)
  out = data.frame(outlier_count = o)
  return(out)
}




