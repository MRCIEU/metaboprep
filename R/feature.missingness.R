#' estimate feature missingness
#'
#' This function estimates feature missingess, with a step to exclude poor samples identified as those with a sample missingness greater than 50%.
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param samplemissingness a vector of sample missingness for each sample
#' @param extreme_sample_mis_threshold a numeric value above which individuals with sample missingness should be excluded from the feature missingess estimator. Default is 0.5.
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
#' smis = apply(ex_data, 1, function(x){ sum(is.na(x))/length(x) })
#' feature.missingness(wdata = ex_data, samplemissingness = smis)
#'
feature.missingness <- function( wdata, samplemissingness = NULL, extreme_sample_mis_threshold = 0.5){

  ## Identify any sample that performed VERY poorly. 
  ## If there are any, remove these sample prior to 
  
  ## Estimating feature missingness
  w = which(samplemissingness >= extreme_sample_mis_threshold)
  if(length(w)>0){
    out = apply(wdata[-w, ], 2, function(x){ 
      o = sum(is.na(x)) / length(x)
      return(o)
    })
    } else {
      out = apply(wdata, 2, function(x){ 
      o = sum(is.na(x)) / length(x)
      return(o)
      })
    }
  out = data.frame(feature_missingness = out)
  return(out)
}




