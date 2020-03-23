#' A Function to evaluate the effect of a laboratory batch variable on all metabolite feautures in a data matrix
#'
#' This function allows you to estimate effects of laboratory batch variable on metabolite features.
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param batch a single vector containing a vector based batch variable
#' @keywords metabolomics
#' @export
#' @examples
#' met2batch()
met2batch = function(wdata, batch){
  batch = as.factor(batch)
  ##
  batch_eta = t( apply( wdata, 2, function(x){
    ## estatimate variance
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
      a = anova(fit)
      ## estimate variance explained Eta-Sq
      eta = a[1,2] / sum(a[,2])
      p = a[1,5]
    }
    return(c(eta, p))
  }) )
  
  ## estimate FDR
  padj = p.adjust(batch_eta[,2], method = "BH")
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




