#' outlier features count for samples
#'
#' This function takes a matrix of numeric data and counts the number of outlying features each sample has.
#'
#' @param wdata a metabolite data matrix with samples in row, metabolites in columns
#' @param nsd the number of standard deviation from the mean outliers are identified at. The default value is 5.
#'
#' @keywords metabolomics outliers
#' 
#' @return a data frame of outiler counts for each sample
#'
#' @export
#'
#' @examples
#' d = sapply(1:5, function(x){ rnorm(50, 50, 15) })
#' sample.outliers(d, nsd = 2)
#'
sample.outliers = function(wdata, nsd = 5){
  Omat = outlier.matrix(wdata, nsd)
  o = apply(Omat, 1, sum)
  out = data.frame(outlier_count = o)
  return(out)
  }




