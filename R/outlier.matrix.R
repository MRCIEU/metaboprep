#' identify outlier sample indexes in a matrix
#'
#' Given a matrix of data this function returns a matrix of 0|1, of the same structure with 1 values indicating outliers. It is an expansion of the function id.outliers(), applied to columns of a matrix.
#'
#' @param wdata a matrix of numerical values, samples in row, features in columns
#' @param nsd the number of standard deviation from the mean outliers are identified at. Default value is 5.
#'
#' @keywords outlier matrix indexes
#' 
#' @return a matrix of 0 (not a sample outlier) and 1 (outlier)
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:25, function(x){ rnorm(250, 40, 5) })
#' ## define the data set
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## add in some technical error to two samples
#' m = apply(ex_data, 2, function(x){ mean(x, na.rm = TRUE) })
#' ex_data[c(1,50), ] = ex_data[1, ] + (m*4) 
#' Omat = outlier.matrix(ex_data)
#' ## how many outliers identified
#' sum(Omat)
#'
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




