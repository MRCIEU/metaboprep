#' A Function to perform a PCA factor analysis. 
#'
#' This function allows you to perform a PCA factor analysis. 
#' @param metabolitedata data frame of metabolite data
#' @param pcloadings data frame of metabolite data
#' @param sigthreshold signficance threshold in spearman correlation units
#' @param feature_anno the feature annotation to include in the factor analysis
#' @keywords metabolomics
#' @export
#' @examples
#' pca.factor.analysis()
pca.factor.analysis = function( metabolitedata, pcloadings, sigthreshold = 0.3, feature_anno = feature_data$SUPER_PATHWAY ){
  
  FA_Cors = t( apply(metabolitedata, 2, function(met){
    out = sapply(1:5, function(pc){
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
      ftest = fisher.test(mat)
      o = c(ftest$estimate, ftest$p.value); names(o) = c("OR","pval")
      out = c(data4matrix, o)
    }) )
    return(EnrichmentTests)
  })
  
  
  return(list( enrichment_tables = enrichment_tables, CorMat = FA_Cors ) )
  
}
