#' PCA factor analysis and annotation enrichment 
#'
#' This function performs (1) a factor analysis on numeric data and PC loadings derived from said data, then subsequently (3) performs a hypergeometrix enrichment test to ask if a certain class of variables are enriched on a particular PC.
#'
#' @param metabolitedata a matrix or data frame of metabolite data
#' @param pcloadings a matrix or data frame of pc loadings you wish to test
#' @param sigthreshold Spearman's rho correlation threshold to declare an association between the numeric variable and a PC loading
#' @param feature_anno a vector of variable annotations to perform the hypergeomtric enrichment on
#'
#' @keywords PC factor analysis hypergeometric enrichment fisher exact test
#' 
#' @importFrom stats fisher.test
#' 
#' @return a list object of length 2: (1) a list object of enrichment_tables, and (2) the Spearman's correlation matrix between features and the PC loadings
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
## define the data set
#' ex_data = cbind(d1,d2,d3)
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## annotation
#' met_anno = c( rep("A", 10), rep("B", 10), rep("C", 8) )
#' ## PCA
#' pca = prcomp(ex_data, center = TRUE, scale = TRUE)
#' ## run pca.factor.analysis()
#' ex_out = pca.factor.analysis(metabolitedata = ex_data, 
#'                              pcloadings = pca$x[,1:5], 
#'                              sigthreshold = 0.3, 
#'                              feature_anno = met_anno )
#' 
#'
pca.factor.analysis = function( metabolitedata, pcloadings, sigthreshold = 0.3, feature_anno = feature_data$SUPER_PATHWAY ){
  ## define local variable 
  feature_data <- NULL
  ##
  FA_Cors = t( apply(metabolitedata, 2, function(met){
    out = sapply(1:ncol(pcloadings), function(pc){
      o = suppressWarnings( cor.test(met, pcloadings[, pc], method = "sp")$estimate )
      return( abs(o))
    })
    return(out)
  }) )
  
  ## categories to be testing for enrichment
  cats = unique(feature_anno)
  
  ## test for enrichment of all PCs provided in the pcloadings matrix
  enrichment_tables = lapply(1:ncol(pcloadings), function(i){
    ## which are and are not "significantly" correalted to the PC
    ## being tested. Significance is declared by the Spearman's Rho
    ## (sigthreshold)
    sig = which(FA_Cors[,i] >= sigthreshold )
    notsig = which(FA_Cors[,i] < sigthreshold )
    ## create a table of counts for 
    ## associated and not associated features
    A = table( feature_anno[sig] )
    B = table( feature_anno[notsig] ) 
    ##  a table of counts for all features
    ALL = table(feature_anno)
    
    ### Create Fisher Exact Table and perform hypergeometric test
    EnrichmentTests = t( sapply(cats, function(cat){
      incat_cor = A[which( names(A) == cat)]
      if(length(incat_cor) == 0){ incat_cor = 0 }
      ##
      test = ALL[which( names(ALL) == cat)]
      incat_NOTcor = test - incat_cor
      if(length(incat_NOTcor) == 0){ incat_NOTcor = 0 }
      ##
      NOTincat_cor = sum(A) - incat_cor
      NOTincat_NOTcor = sum( ALL[which( names(ALL) != cat)] ) - sum(A[which( names(A) != cat)])
      
      data4matrix = c(incat_cor, incat_NOTcor, NOTincat_cor, NOTincat_NOTcor )
      names(data4matrix) = c("InCat_Associated","InCat_NotAssociated","NotInCat_Associated","NotInCat_NotAssociated")
      mat = matrix( data4matrix , 2, 2, byrow = TRUE )
      ftest = stats::fisher.test(mat)
      o = c(ftest$estimate, ftest$p.value); names(o) = c("OR","pval")
      out = c(data4matrix, o)
    }) )
    return(EnrichmentTests)
  })
  
  
  return(list( enrichment_tables = enrichment_tables, CorMat = FA_Cors ) )
  
}
