#' A Function to identify outliers from a vector of data at SD units from the mean.
#'
#' This function to identify outliers from a vector of data at SD units from the mean.
#' @param x a numerical vector of data
#' @param nsd the Number of SD units from the mean to be used as an outlier cutoff. 
#' @keywords metabolomics
#' @export
#' @examples
#' outliers()
outliers = function(x, nsd = 3){
  # x is a vector of values
  x = as.numeric(x)
  m = mean(na.omit(x))
  s = sd(na.omit(x))
  bottom = m-(nsd*s)
  top = m+(nsd*s)
  cuttoffs = c(top, bottom)
  out = c(which(x > cuttoffs[1]), which(x < cuttoffs[2]))
  output = list(out,bottom,top)
  return(output)
}

