#' principal component analysis
#'
#' This function performs two principal component analysis. In the first, missing data is imputed to the median. In the second a probablistic PCA is run to account for the missingness. 
#' Subsequent to the derivation of the PC, the median imputed PC data is used to identify the number of informative or "significant" PC by (1) an acceleration analysis, and (2) a parrallel analysis.
#' Finally the number of sample outliers are determined at 3, 4, and 5 standard deviations from the mean on the top PCs as determined by the acceleration factor analysis. 
#'
#' @param metabolitedata the metabolite data matrix. samples in row, metabolites in columns
#' @param indfeature_names a vector of independent feature names | column names. 
#' @param outliers  defaulted to TRUE, a TRUE|FALSE binary flagging if you would like outliers identified. 
#'
#' @keywords PCA probalistic PCA
#'
#' @importFrom stats prcomp
#' @importFrom pcaMethods ppca
#' @importFrom nFactors parallel nScree
#' 
#' @return a list object of length five, with (1) a data frame of PC loadings, (2) a vector of variance explained estimates for each PC, (3) an estimate of the number of informative or top PCs determined by the acceleration factor analysis, (4) an estimate of the number of informative or top PCs determined by parrallel analysis, (5) a data frame of the probablisitic PC loadings
#'
#' @export
#'
#' @examples
#' ## define a covariance matrix
#' cmat = matrix(1, 4, 4 )
#' cmat[1,] = c(1, 0.8, 0.6, 0.2)
#' cmat[2,] = c(0.8, 1, 0.7, 0.5)
#' cmat[3,] = c(0.6, 0.7, 1, 0.6)
#' cmat[4,] = c(0.2, 0.5, 0.6,1)
#' ## simulate some correlated data (multivariable random normal)
#' set.seed(1110)
#' d1 = MASS::mvrnorm(n = 250, mu = c(5, 45, 25, 15), Sigma = cmat )
#' set.seed(1010)
#' d2 = MASS::mvrnorm(n = 250, mu = c(5, 45, 25, 15), Sigma = cmat )
#' ## simulate some random data
#' d3 = sapply(1:20, function(x){ rnorm(250, 40, 5) })
#' ## define the data set
#' ex_data = cbind(d1,d2,d3)
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## add in some missingness
#' ex_data[sample(1:length(ex_data), 450)] = NA
#' ## add in some technical error to two samples
#' m = apply(ex_data, 2, function(x){ mean(x, na.rm = TRUE) })
#' ex_data[c(1,10), ] = ex_data[1, ] + (m*0.00001) 
#' ## run the PCA
#' ex_out = pc.and.outliers(ex_data, indfeature_names = sample(colnames(ex_data), 15) )
#' 
#'
pc.and.outliers = function(metabolitedata, indfeature_names, outliers = TRUE ){
  ## package check
  pkgs = c("stats", "pcaMethods", "nFactors")
  for(pkg in pkgs){
    if (!requireNamespace( pkg, quietly = TRUE)) {
        stop(paste0("Package \"", pkg,"\" needed for pc.and.outliers() function to work. Please install it."),call. = FALSE)
      }
  }
  
  ## set data
  w = which( colnames(metabolitedata) %in% indfeature_names )
  indf = colnames(metabolitedata)[w]
  pcadata = metabolitedata[, indf ]
  prob_pcadata = metabolitedata[, indf ]
  
  ##############################
  ## impute missingness as medians
  ##############################
  pcadata = median_impute(wdata = pcadata)
  rownames(pcadata) = rownames(metabolitedata)
  
  ##############################
  ## z-transformation
  ##############################
  pcadata = apply(pcadata, 2, function(x){
    ( x - mean(x, na.rm = TRUE) ) / sd(x, na.rm = TRUE)
  })
  ##############################
  ## perform PCA
  ##############################
  mypca = stats::prcomp(pcadata, center = FALSE, scale = FALSE)
  varexp = summary(mypca)[[6]][2, ]
  
  ###############################
  ## perform probablisitic PCA
  ###############################
  prob_pcadata = apply(prob_pcadata, 2, function(x){
    ( x - mean(x, na.rm = TRUE) ) / sd(x, na.rm = TRUE)
  })
  ######
  prob_mypca = pcaMethods::ppca(prob_pcadata, method="ppca", nPcs = 10, seed = 1234 , maxIterations = 1000)

  ##############################
  ## find number of sig PCs
  ##############################
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
  
  ## build list to return
  dataout = list(pcs = PCout, varexp = varexp, accelerationfactor = accelerationfactor, nsig_parrallel = nsig_parrallel, prob_pca = prob_mypca@scores  )

  return(dataout)
}


