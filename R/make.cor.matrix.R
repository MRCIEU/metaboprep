#' correlation matrix
#'
#' This function estimates a correlation matrix returning wither the correlation estimates or their p-values
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param cor_method defaulted to "kendall" this is the correlation method to use in the function cor.test()
#' @param minN sefaulted to 50, this is the minimum number of observations that must be available in pairs to perform analysis
#' @param var2return sefaulted to "cor", other option is "pvalue" is a the flag indicating which estimate to return from the function.
#'
#' @keywords correlation matrix
#'
#' @importFrom stats cor.test
#' 
#' @return a matrix of correlation estimates or p-values
#'
#' @export
#'
#' @examples
#' cmat = matrix(1, 4, 4 )
#' cmat[1,] = c(1, 0.8, 0.6, 0.2)
#' cmat[2,] = c(0.8, 1, 0.7, 0.5)
#' cmat[3,] = c(0.6, 0.7, 1, 0.6)
#' cmat[4,] = c(0.2, 0.5, 0.6,1)
#' ## simulate some correlated data (multivariable random normal)
#' set.seed(1110)
#' ex_data = MASS::mvrnorm(n = 250, mu = c(5, 45, 25, 15), Sigma = cmat )
#' ## return correlation estimates
#' cor_mat = make.cor.matrix(ex_data, var2return = "cor")
#' ## return p-values
#' cor_mat = make.cor.matrix(ex_data, var2return = "pvalue")
#'
make.cor.matrix = function(wdata, cor_method = "kendall", minN = 50, var2return = "cor"){
  matout = matrix( NA, nrow = ncol(wdata), ncol = ncol(wdata) )
 ### 
  for(i in 1:ncol(wdata)){
    for(j in 1:ncol(wdata)){
      temp_data = na.omit( data.frame(a = wdata[,i], b = wdata[, j]) )

      if( nrow(temp_data) >= minN){
        cor_out = stats::cor.test(temp_data$a, temp_data$b, method = cor_method)
        if(var2return == "cor"){
            out = cor_out$estimate
          } else {
            if(var2return == "pvalue"){
              out = cor_out$p.value
            } else {
              out = cor_out$estimate
            }
          }
          
      } else {
        out = NA
      }
      matout[i,j] = out
    }
  }
  return(matout)
}


