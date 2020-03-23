#' A Function to provide a count of the number outlying features for a sample
#'
#' This function takes a matrix of metaboite data (samples in rows, features in columns) and counts the number of outlying features each sample has.
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param nsd the number of standard deviation from the mean outliers are identified at. Default value is 5.
#' @keywords metabolomics
#' @export
#' @examples
#' sample.outliers()
sample.outliers = function(wdata, nsd = 5){
  Omat = outlier.matrix(wdata, nsd)
  o = apply(Omat, 1, sum)
  out = data.frame(outlier_count = o)
  return(out)
  }




