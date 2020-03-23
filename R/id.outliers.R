#' A Function to identify SD outliers in a vector of data
#'
#' given a vector of data, identify those positions that are standard deviation outliers in that vector of data
#' @param x a vector of numerical values
#' @param nsd the number of standard deviation from the mean outliers are identified at. Default value is 5.
#' @keywords metabolomics
#' @export
#' @examples
#' id.outliers()
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




