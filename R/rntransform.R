#' rank normal tranformation
#'
#' This function rank normal transforms a vector of data. The procedure is built off of that provided in the GenABEL pacakge.
#'
#' @param y a numeric vector which will be rank normal transformed
#' @param split_ties a binary string of TRUE (default) or FALSE indicating if tied values, of the same rank, should be randomly split giving them unique ranks.
#'
#' @keywords rank normal transformation
#'
#' @importFrom stats qnorm
#'
#' @return returns a numeric vector, with the same length as y, of rank normal transformed values
#'
#' @export
#'
#' @examples
#' ## simulate a negative binomial distribution of values
#' nb_data = rnbinom(500, mu = 40, size = 100)
#' ## rank normal transform those values
#' rnt_data = rntransform( nb_data , split_ties = TRUE )
#'
rntransform = function( y, split_ties = TRUE ){
  ## verify that x is a vector.
  if(is.vector(y) == FALSE){ stop("id_outliers() parameter y is expected to be a vector of data.") }

  ## verify some variablity
  if (length(unique(y)) == 1){ stop("trait is monomorphic") }

  if (length(unique(y)) == 2){ stop("trait is binary") }

  ## identify and remove the NAs
  w = (!is.na(y))
  x <- y[w]

  ## empty vector of Z values
  z = rep(NA, length(w))

  ## z-transform
  temp <- (x - mean(x))/sd(x)

  ## define z values for all observations including the NAs
  z[w] = temp

  ## rank the data
  if(split_ties == TRUE){
    rnt <- rank(z, ties.method = "random") - 0.5
  } else {
    rnt <- rank(z) - 0.5
  }
  ## insure the NAs remain so
  rnt[is.na(z)] <- NA

  ## inverse
  rnt <- rnt/(max(rnt, na.rm = T) + 0.5)
  ## quantile normalize
  rnt <- stats::qnorm(rnt)

  return(rnt)
}
