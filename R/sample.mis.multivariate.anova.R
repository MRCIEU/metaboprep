#' This function estimate the effect of run day and box-id in a multivatiate model.
#'
#' This function estimate the effect of run day and box-id in a multivatiate model.
#' @param sdata A tibble of sample summary stats and batch variables. 
#' @keywords metabolomics
#' @export
#' @examples
#' sample.mis.multivariate.anova()
sample.mis.multivariate.anova = function(sdata){
  ## fit the model
  fit = lm(sample_missing ~ as.factor(BOX_ID) + as.factor(RUN_DAY), data = sdata)
  
  ## Type I ANOVA
  a1 = anova(fit)
  eta1 = a1[1:2,2] / sum(a1[,2])
  pval1 = a1[1:2, 5]
  
  ## Type I ANOVA
  a2 = Anova(fit, type = "II")
  eta2 = a2[1:2,1] / sum(a2[,1])
  pval2 = a2[1:2, 4]
  
  ## Make an reporting table
  outtable = as_tibble( matrix( c(eta1, pval1, eta2, pval2), nrow = 2, byrow = TRUE, 
                                dimnames = list( c("TypeI_ANOVA","TypeII_ANOVA"), 
                                                 c("BoxID_etasq","RunDay_etasq","BoxID_pval","RunDay_pval")  ) ) )
  outtable = outtable %>% mutate(AnovaType = c("TypeI_ANOVA","TypeII_ANOVA")) %>% select(AnovaType, everything())
  ## make the table a ggplot like figure
  outtable <- ggtexttable(outtable, rows = NULL, 
                                       theme = ttheme("mBlue"))
  ## return output
  return(outtable)
}

