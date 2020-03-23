#' A Function to evaluate and plot batch effects on sample missingness.
#'
#' This function allows you to evaluate BOXID and RUNDAY on missingness and return a summary plot.
#' @param sdata A tibble of metabolute sample summary stats and sample batch data
#' @keywords metabolomics
#' @export
#' @examples
#' sample.missingness.by.batch()
sample.missingness.by.batch = function(sdata){
  ###########################
  ## Box ID on Missingness
  ###########################
  fit = lm(sample_missing ~ BOX_ID, data = sdata)
  a = anova(fit)
  eta = signif( a[1,2]/sum(a[,2]), d = 2 )
  pval = signif( a[1, 5], d = 3 )
  ###
  plotA = sdata  %>% ggplot( aes(x = BOX_ID, y = sample_missing)) +
    #geom_boxplot( aes(color = BOX_ID), notch = TRUE) +
    geom_violin( aes(color = as.factor(BOX_ID) ) ) +
    geom_dotplot(aes(color = as.factor(BOX_ID)), 
                 binaxis='y', stackdir='center', 
                 dotsize=0.35, alpha= 0.6,
                 binwidth = 0.006) +
    theme( axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(title = "Sample Missingness by BOX ID", 
         subtitle = paste0( "   ---  " ,  
                            eta ,
                            "% of the varition in sample misingness can be explained by BOX ID (pval = ", 
                            pval, ")."),
         y = "Missingness", x = "Box ID", color = "Box ID")
  
  ###########################
  ## Run day on Missingness
  ###########################
  fit = lm(sample_missing ~ as.factor(RUN_DAY), data = sdata)
  a = anova(fit)
  eta = signif( a[1,2]/sum(a[,2]), d = 2 )
  pval = signif( a[1, 5], d = 3 )
  ###
  plotB = sdata  %>% ggplot( aes(x = as.factor(RUN_DAY), y = sample_missing)) +
    #geom_boxplot( aes(color = as.factor(RUN_DAY) ), notch = TRUE) +
    geom_violin( aes(color = as.factor(RUN_DAY) ) ) +
    geom_dotplot(aes(color = as.factor(RUN_DAY)) ,
                 binaxis='y', stackdir='center', 
                 dotsize=0.35, alpha= 0.6,
                 binwidth = 0.006) +
    theme( axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(title = "Sample Missingness by RUN DAY", 
         subtitle = paste0( "   ---  " , 
                            eta ,
                            "% of the varition in sample misingness can be explained by RUN DAY (pval = ", 
                            pval, ")."),
         y = "Missingness", x = "Run Day", color = "Run Day")
  
  ### place both plots in a list
  toplot = list( plotA = plotA, plotB = plotB)
  
  ## return plots to the user
  return(toplot)
}
