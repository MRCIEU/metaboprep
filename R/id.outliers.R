#' identify outliers
#'
#' given a vector of data, identify those positions that are 'nsd' standard deviation units from the mean
#'
#' @param x a vector of numerical values
#' @param nsd the number of standard deviation from the mean outliers are identified at. Default value is 5.
#'
#' @keywords outliers
#' 
#' @return a vector indexing which samples are outliers
#'
#' @export
#'
#' @examples
#' ex_data = rnbinom(500, mu = 40, size = 5)
#' id.outliers(ex_data, nsd = 2)
#'
id.outliers = function(x, nsd = 5){
  msd = c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
  cutoff = msd[1] + (msd[2]*nsd)
  ####
  w = which(x > cutoff | x < -cutoff)
  if(length(w)>0){
    return(w)
  } else {
    return(NA)
    }
  }




