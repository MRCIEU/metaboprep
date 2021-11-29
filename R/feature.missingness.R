#' estimate feature missingness
#'
#' This function estimates feature missingess, with a step to exclude poor samples identified as those with a sample missingness greater than 50%.
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#'
#' @keywords feature missingness
#' 
#' @return a data frame of percent missingness for each feature
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:5, function(x){rnorm(10, 45, 2)})
#' ex_data[ sample(1:length(ex_data), 15) ] = NA
#' feature.missingness(wdata = ex_data )
#'
feature.missingness <- function( wdata ){

  out = apply(wdata, 2, function(x){ 
    o = sum(is.na(x)) / length(x)
    return(o)
  })

  # make a data frame
  out = data.frame(feature_missingness = out)
  ## return data
  out
}




