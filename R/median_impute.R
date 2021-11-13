#' median impute missing data
#'
#' This function imputes features (columns) of a metabolome matrix to median estimates. Useful for PCA.
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#'
#' @keywords metabolomics median imputation
#' 
#' @return the matrix passed to the function but with NA's imputed to each columns median value.
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:100, function(x){ rnorm(250, 40, 5) })
#' ## define the data set
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## add in some missingness
#' ex_data[sample(1:length(ex_data), 500)] = NA
#' ## Estimate missingness and generate plots
#' imp_data = median_impute(ex_data)
#'
median_impute = function( wdata ){
  rn = rownames(wdata)
  cn = colnames(wdata)
  out = sapply( 1:ncol(wdata), function(i){
      x = as.numeric( wdata[, i]  )
      m = median(x, na.rm = TRUE)
      w = which(is.na(x))
      x[w] = m
      return( t(x) )
    })
  rownames(out) = rn
  colnames(out) = cn
  return(out)
  }


