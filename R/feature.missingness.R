#' A Function to estiamte missingness on samples,
#'
#' This function allows you to estimate feature missingess, with a step to exclude poor samples identified as those with a sample missingness greater than 50%.
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param samplemissingness a vector of sample missingness for each sample
#' @keywords metabolomics
#' @export
#' @examples
#' feature.missingness()
feature.missingness <- function(wdata, samplemissingness){
  ## identify any sample that performed VERY poorly. 
  ## if there are any, remove these sample prior to 
  ## estimating feature missingness
  w = which(samplemissingness > 0.5)
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




