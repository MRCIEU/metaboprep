#' outlier sample count for a features
#'
#' This function takes a matrix of data (samples in rows, features in columns) and counts the number of outlying samples each feature has.
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param nsd the number of standard deviation from the mean outliers are identified at. Default value is 5.
#'
#' @keywords feature outliers
#' 
#' @return a data frame out sample outlier counts for each feature (column) in the matrix
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:20, function(x){ rnorm(250, 40, 5) })
#' s = sample(1:length(ex_data), 200)
#' ex_data[s] = ex_data[s] + 40
#' ## run the function
#' fout = feature.outliers(ex_data)
#'
feature.outliers = function(wdata, nsd = 5){
  Omat = outlier.matrix(wdata, nsd)
  o = apply(Omat, 2, sum)
  out = data.frame(outlier_count = o)
  return(out)
}




