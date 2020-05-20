#' A Function to estimate PC and then identify possible outliers
#'
#' This function allows you to estiamte PCs and identify outliers in those PCs.
#' @param metabolitedata the metabolite data matrix. samples in row, metabolites in columns
#' @param indfeature_names a vector of independent feature names | column names. 
#' @param outliers  Defaulted to TRUE, a TRUE|FALSE paramater flagging if you would like outliers identified. 
#' @keywords metabolomics
#' @export
#' @examples
#' pc.and.outliers()
pc.and.outliers = function(metabolitedata, indfeature_names, outliers = TRUE ){
  library(MASS)
  #if( length(indfeatures) != ncol(metabolitedata) ){
  #  stop( 
  #    paste0("The length of indfeatures does not equal the number of features (ncol) in the metabolite matrix.\nindfeatures should be a vector of 0|1, where 1 indicates yes, this is an 
  #       independent feature")
  #  )
  #}
  
  ## set data
  w = which(colnames(metabolitedata) %in% indfeature_names)
  indf = colnames(metabolitedata)[w]
  pcadata = metabolitedata[, indf ]
  
  ## filter on missingness
  # mis = apply(pcadata, 2, function(x){ sum(is.na(x))  }) 
  # r = which(mis>0)
  # if(length(r) > 0){
  #   pcadata = pcadata[, -r]
  # }
  # 
  
  ## impute missingness as medians
  pcadata = median_impute(wdata = pcadata)
  rownames(pcadata) = rownames(metabolitedata)
  ## estimate PCs
  ## z-transformation
  pcadata = apply(pcadata, 2, function(x){
    ( x - mean(x, na.rm = TRUE) ) / sd(x, na.rm = TRUE)
  })
  mypca = stats::prcomp(pcadata, center = FALSE, scale = FALSE)
  varexp = summary(mypca)[[6]][2, ]
  
  ## find number of sig PCs
  ev <- eigen(cor(pcadata)) # get eigenvalues
  ap <- nFactors::parallel(subject=nrow(pcadata),var=ncol(pcadata),
                 rep=100,cent=.05)
  nS <- nFactors::nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  accelerationfactor = as.numeric( nS[[1]][2] )
  if(accelerationfactor < 2) { accelerationfactor = 2 }
  nsig_parrallel = nS[[1]][3]
  ####
  
  if(outliers == TRUE){
    
    ## identify outliers
    Omat3 = outlier.matrix(mypca$x[, 1:accelerationfactor], nsd = 3)
    colnames(Omat3) = paste0(colnames(Omat3), "_3_SD_outlier")
    PCout = cbind(mypca$x[,1:10], Omat3)
    ####
    Omat4 = outlier.matrix(mypca$x[, 1:accelerationfactor], nsd = 4)
    colnames(Omat4) = paste0(colnames(Omat4), "_4_SD_outlier")
    PCout = cbind(PCout, Omat4)
    ####
    Omat5 = outlier.matrix(mypca$x[, 1:accelerationfactor], nsd = 5)
    colnames(Omat5) = paste0(colnames(Omat5), "_5_SD_outlier")
    PCout = cbind(PCout, Omat5)
  } else {
    PCout = mypca$x[,1:10]
  }
  

  dataout = list(pcs = PCout, varexp = varexp, accelerationfactor = accelerationfactor, nsig_parrallel = nsig_parrallel  )
  
  # detach("package:MASS", unload=TRUE)

  return(dataout)
}


