#' @title Identify indexes of outliers in data
#' @description
#' Given a vector or matrix, this function returns a vector or matrix of 0|1, of the same structure with 1 values indicating outliers.
#' @param data a matrix of numerical values, samples in row, features in columns
#' @param nsd the unit distance in SD or IQR from the mean or median estimate, respectively outliers are identified at. Default value is 5.
#' @param meansd set to TRUE if you would like to estimate outliers using a mean and SD method; set to FALSE if you would like to estimate medians and inter quartile ranges. The default is FALSE.
#' @param by character, either 'column' to compute along columns or 'row' to compute across rows. Irrelevant for vectors.
#'
#' @return a matrix of 0 (not a sample outlier) and 1 (outlier)
#'
#' @importFrom stats quantile
#' @export
#'
outlier_detection <- function(data, nsd = 5, meansd = FALSE, by = "column") {

  by <- match.arg(by, choices = c("row", "column"))

  if (is.vector(data)) {
    return(outlier_vector(data, nsd, meansd))
  } else if (is.matrix(data)) {
    if (by == "row") {
      out <- apply(data, 1, function(x) outlier_vector(x, nsd, meansd))
      rownames(out) <- colnames(data)
    } else {
      out <- apply(data, 2, function(x) outlier_vector(x, nsd, meansd))
      rownames(out) <- rownames(data)
    }
    return(out)
  } else {
    stop("Input must be a vector or a matrix.")
  }
}

outlier_vector <- function(x, nsd, meansd) {
  if (meansd) {
    msd <- c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
    cutoff <- c(msd[1] - (msd[2] * nsd), msd[1] + (msd[2] * nsd))
  } else {
    m <- median(x, na.rm = TRUE)
    p <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- p[2] - p[1]
    cutoff <- c(m - (nsd * iqr), m + (nsd * iqr))
  }

  dataout = rep(0, length(x))
  w = which(x >= cutoff[2] | x <= cutoff[1] )
  if(length(w)> 0){
    dataout[w] = 1
  }
  return(dataout)
}



#' Identify Outliers
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

