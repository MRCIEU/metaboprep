#' A Function to estimate sample missingness
#'
#' This function allows estiamte sample missingness in a matrix of data and provides an option to exclude certain columns or metabolite features from the analysis, such as xenobiotics
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param excludethesefeatures a vector of feature names (i.e. column names) to exclude from missingness estimates
#' @keywords metabolomics
#' @export
#' @examples
#' sample.missingness()
sample.missingness <- function(wdata, excludethesefeatures = NA){
  total_missingness = apply(wdata, 1, function(x){
    o = sum(is.na(x)) / length(x)
    return(o)
    })
  if( !is.na(excludethesefeatures[1]) == TRUE ){
    
    ## which columns to exclude ??
    r = which(colnames(wdata) %in% excludethesefeatures)
    ## estiamte missingness
    limited_missingness = apply(wdata[, -r ], 1, function(x){
      o = sum(is.na(x)) / length(x)
      return(o)
    
      })
    out = data.frame(sample_missingness = total_missingness, sample_missingness_w_exclusions = limited_missingness)
    return( out )
  } else {
      out = data.frame(sample_missingness = total_missingness)
        return( out )
    }
}




