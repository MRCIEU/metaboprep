#' A Function to perform multivariate analysis over a dependent and an independent variable
#'
#' This function performs univariate analysis over a dependent and an independent variable
#' @param dep a vector of the dependent variable
#' @param indep_df a data frame of the independent variable
#' @keywords metabolomics
#' @export
#' @examples
#' multivariate.anova()
multivariate.anova = function(dep, indep_df){
  wdat = data.frame( cbind(indep_df, dep) )
  ## fit the model
  fit = lm(dep ~ . , data = wdat)
  
  
  ## Type I ANOVA
  a = car::Anova(fit, type = "II")
  eta = round( a[ ,1] / sum(a[,1]), d = 4 )*100
  names(eta) = paste0( rownames(a) ,"_eta")
  pval = c( formatC( a[-nrow(a), 4], format = "e", digits = 2) , "NA")
  names(pval) = paste0( rownames(a) ,"_pval")
  
  ## Make an reporting table
  outtable = tibble::as_tibble( matrix( c(eta, pval), ncol = 2, byrow = FALSE, 
                                dimnames = list( c(rownames(a)), 
                                                 c("etasq_VarExp","pvalue")  ) ) )
  outtable = outtable %>% mutate(Batch_Variable = rownames(a) ) %>% dplyr::select(Batch_Variable, everything())
  ## make the table a ggplot like figure
  outtable <- ggpubr::ggtexttable(outtable, rows = NULL, 
                          theme = ggpubr::ttheme("mBlue"))
  ## return output
  return(outtable)
}

