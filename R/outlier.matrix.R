#' A Function to identify outliers in each column of a matrix
#'
#' Given a matrix of data this function returns a matrix of 0|1, of the same structure with 1 values indicating outliers. It is an expansion of the function id.outliers(), applied to columns of a matrix.
#' @param x a vector of numerical values
#' @param nsd the number of standard deviation from the mean outliers are identified at. Default value is 5.
#' @keywords metabolomics
#' @export
#' @examples
#' outlier.matrix()
outlier.matrix = function(wdata, nsd = 5){
  out = apply(wdata, 2, function(x){
    msd = c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
    cutoff = msd[1] + (msd[2]*nsd)
    ##
    dataout = rep(0, length(x))
    w = which(x >= cutoff | x <= -cutoff)
    if(length(w)> 0){
      dataout[w] = 1
    } 
    return(dataout)
    })
  }




