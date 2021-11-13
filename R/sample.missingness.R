#' estimate sample missingness
#'
#' This function estimates sample missingness in a matrix of data and provides an option to exclude certain columns or features from the analysis, such as xenobiotics (with high missingness rates) in metabolomics data sets.
#'
#' @param wdata a numeric matrix with samples in row and features in columns
#' @param excludethesefeatures a vector of feature names (i.e. column names) to exclude from missingness estimates
#'
#' @keywords missingness
#' 
#' @return A data frame of missingness estimates for each sample. If a vector of feature names was also passed to the function a second column of missingness estimates will also be returned providing missingness estimates for each sample to the exclusion of those features provided.
#'
#' @export
#'
#' @examples
#' ## simulate some data
#' set.seed(1110)
#' ex_data = sapply(1:5, function(x){ rnorm(10, 40, 5) })
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## add some missingness to the data
#' ex_data[ sample(1:50, 10) ] = NA
#' ## estimate missingness
#' mis_est = sample.missingness(ex_data)
#' mis_est_v2 = sample.missingness(ex_data, excludethesefeatures = "var5")
#'
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




