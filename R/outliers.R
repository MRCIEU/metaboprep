#' identify outliers
#'
#' This function identifies outliers from a vector of data at SD units from the mean.
#'
#' @param x a numerical vector of data
#' @param nsd the number of SD units from the mean to be used as an outlier cutoff. 
#'
#' @keywords outliers
#' 
#' @return a list object of length three. (1) a vector of sample indexes indicating the outliers, (2) the lower outlier cuttoff value, (3) the upper outlier cuttoff value.
#'
#' @export
#'
#' @examples
#' ex_data = rnbinom(500, mu = 40, size = 5)
#' outliers(ex_data)
#'
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

