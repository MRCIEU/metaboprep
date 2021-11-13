#' batch effect on numeric matrix
#'
#' This function estimates the effects of a categorical (batch) variable on a matrix of features in univariate linear models.
#'
#' @param wdata the numeric data matrix with samples in row, features in columns
#' @param batch a single vector containing a vector based batch variable
#'
#' @keywords metabolomics univariate linear models
#' 
#' @importFrom stats anova p.adjust
#' 
#' @return a list object of length two with (1) a data frame of summary statistics on (a) the number of tested features ,(b) the mean batch effect across all features ,(c)  the mean batch effect across all associated (BH FDR<0.05) features ,(d) the number of associated (BH FDR<0.05) features.
#'
#' @export
#'
#' @examples
#' d1 = sapply(1:10, function(x){ rnorm(50, 30, 5) })
#' d2 = sapply(1:10, function(x){ rnorm(50, 40, 5) })
#' ex_data = rbind(d1, d2)
#' d3 = sapply(1:10, function(x){ rnorm(100, 40, 5) })
#' ex_data = cbind(ex_data, d3)
#' lot = c( rep("A",50), rep("B",50) )
#' ex = met2batch(wdata = ex_data, batch = lot)
#'
met2batch = function(wdata, batch){
  batch = as.factor(batch)
  ##
  batch_eta = t( apply( wdata, 2, function(x){
    ## estimate variance
    v = var(x, na.rm = TRUE)
    ## insure that any featuere with no variation, is NA for any reason, or
    ## has less than 20 observations is not analyzed
    if( sum(!is.na(x), na.rm = TRUE) < 20 | v == 0 | is.na(v) ){
      eta = NA
      p = NA
    } else {
      ## rntransform the data feature, with ties randomly split
      z = rntransform(x)
      fit = lm( z ~ batch )
      ## perform an Type I ANOVA
      a = stats::anova(fit)
      ## estimate variance explained Eta-Sq
      eta = a[1,2] / sum(a[,2])
      p = a[1,5]
    }
    return(c(eta, p))
  }) )
  
  ## estimate FDR
  padj = stats::p.adjust(batch_eta[,2], method = "BH")
  batch_eta = cbind(batch_eta, padj)
  colnames(batch_eta) = c("eta","pval","padj")
  ##
  mean_effect_for_all_features = mean(batch_eta[,1], na.rm = TRUE)
  ## which features are significant
  w = which(batch_eta[,3] < 0.05)
  if( length(w) == 0){
    count_of_sig_features = 0
    mean_effect_for_sig_features = 0
  } else{
    count_of_sig_features = length(w)
    mean_effect_for_sig_features = mean(batch_eta[w,1], na.rm = TRUE)
  }
  count_of_tested_features = sum(!is.na(batch_eta[,2]), na.rm = TRUE)
  outA = data.frame(count_of_tested_features = count_of_tested_features,
                   mean_effect_for_all_features = mean_effect_for_all_features,
                   mean_effect_for_sig_features = mean_effect_for_sig_features,
                   count_of_features_sig_infl_by_batch = count_of_sig_features)
  
  out = list(outA , batch_eta )
  return(out)
}




