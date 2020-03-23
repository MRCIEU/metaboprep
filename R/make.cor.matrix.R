#' A fully bespoke function to generate a correaltion matrix of either correlation estimates or the pvalues
#'
#' This function allows estiamte sample missingness in a matrix of data and provides an option to exclude certain columns or metabolite features from the analysis, such as xenobiotics
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param cor_method Defaulted to "kendall" this is the correlation method to use in the function cor.test()
#' @param minN Defaulted to 50, this is the minimum number of observations that must be available in pairs to perform analysis
#' @param var2return Defaulted to "cor", other option is "pvalue" is a the flag indicating which estimate to return from the function.
#' @keywords nigtingale
#' @export
#' @examples
#' make.cor.matrix()
make.cor.matrix = function(wdata, cor_method = "kendall", minN = 50, var2return = "cor"){
  matout = matrix( NA, nrow = ncol(wdata), ncol = ncol(wdata) )
 ### 
  for(i in 1:ncol(wdata)){
    for(j in 1:ncol(wdata)){
      temp_data = na.omit( data.frame(a = wdata[,i], b = wdata[, j]) )

      if( nrow(temp_data) >= minN){
        cor_out = cor.test(temp_data$a, temp_data$b, method = cor_method)
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


