#' A Function to impute features (columns) of a metabolome matrix to median estimates
#'
#' This function allows you  to impute features (columns) of a metabolome matrix to median estimates. Useful for PCA.
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @keywords metabolomics
#' @export
#' @examples
#' median_impute()
median_impute = function( wdata ){
  out = sapply( 1:ncol(wdata), function(i){
      x = as.numeric( wdata[, i]  )
      m = median(x, na.rm = TRUE)
      w = which(is.na(x))
      x[w] = m
      return( t(x) )
    })
  return(out)
  }


