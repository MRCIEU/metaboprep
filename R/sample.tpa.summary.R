#' A Function to summarizes TPA and missingness in plots
#'
#' This function allows you to ...
#' @param mydata matrix of metabolite abundances, features in columns
#' @param sdata sample summary statistics and batch variables
#' @keywords metabolomics
#' @export
#' @examples
#' sample.tpa.summary()
sample.tpa.summary = function(mydata, sdata){
  
  ## correlation between the two estimates of TPA
  tpa_cor = cor.test(sdata$sample_tpa_total, sdata$sample_tpa_complete, method = "s")
  
  ## correlation between TPA and missingness
  tpa_mis_cor = cor.test(sdata$sample_tpa_total, sdata$sample_missing, method = "s")
  tpaF_mis_cor = cor.test(sdata$sample_tpa_complete, sdata$sample_missing, method = "s")
  
  ## Identify Sample outliers with TPA from complete features only
  ## at SD of 3,4,and 5
  s_outliers <- lapply(3:5, function(x){outliers(sdata$sample_tpa_complete,x)})
  
  
  ############################
  ## make figures evauating
  ## the correlation between
  ## TPA and missingness
  ##
  ############################
  plotA = sdata %>% ggplot(aes(x = sample_tpa_total, y = sample_tpa_complete)) +
    geom_point(color = pcol[2], size = 3.5) +
    geom_smooth(method = loess, color = pcol[1], size = 2) +
  labs(title = "Total Peak Area: all features by complete features", 
       x = "TPA for all features", y = "TPA for complete features",
       caption = paste0("Spearman's cor = ", 
                        signif(tpa_cor$estimate, d = 4), ", pval = ", 
                        signif(tpa_cor$p.value ,d = 4)) )
  
  plotB = sdata %>% ggplot(aes(x = sample_missing, y = sample_tpa_complete)) +
    geom_point(color = pcol[2], size = 1.5) +
    geom_smooth(method = loess, color = pcol[1], size = 2) +
    geom_hline(yintercept = s_outliers[[1]][[3]], linetype = 2, size = 0.5, col = "orange") +
    geom_hline(yintercept = s_outliers[[2]][[3]], linetype = 2, size = 0.5, col = "orange3") +
    geom_hline(yintercept = s_outliers[[3]][[3]], linetype = 2, size = 0.5, col = "red") +
    geom_hline(yintercept = s_outliers[[1]][[2]], linetype = 2, size = 0.75, col = "dodgerblue") +
    #geom_hline(yintercept = s_outliers[[2]][[2]], linetype = 2, size = 0.75, col = "navyblue") +
    #geom_hline(yintercept = s_outliers[[3]][[2]], linetype = 2, size = 0.75, col = "purple") +
  labs(title = "Complete feature TPA by sample missingness", 
       x = "sample missingness", y = "TPA for complete features",
       caption = paste0("Spearman's cor = ", 
                        signif(tpaF_mis_cor$estimate, d = 4), ", pval = ", 
                        signif(tpaF_mis_cor$p.value ,d = 4)) )
  
  ############################
  ## make figures to associate
  ## TPA (complete features) 
  ## with boxID and runday
  ############################
  fit = lm(sample_tpa_complete ~ BOX_ID, data = sdata)
  a = anova(fit)
  eta = signif( a[1,2]/sum(a[,2]), d = 2 )
  pval = signif( a[1, 5], d = 3 )
  ###
  plotC = sdata  %>% ggplot( aes(x = BOX_ID, y = sample_tpa_complete)) +
    #geom_boxplot( aes(color = BOX_ID), notch = TRUE) +
    geom_violin( aes(color = as.factor(BOX_ID) ) ) +
    geom_dotplot(aes(color = as.factor(BOX_ID) ), 
                 binaxis='y', stackdir='center', 
                 dotsize=0.35, alpha= 0.6) +
    geom_hline(yintercept = s_outliers[[1]][[3]], linetype = 2, size = 0.75, col = "orange") +
    geom_hline(yintercept = s_outliers[[2]][[3]], linetype = 2, size = 0.75, col = "orange3") +
    geom_hline(yintercept = s_outliers[[3]][[3]], linetype = 2, size = 0.75, col = "red") +
    geom_hline(yintercept = s_outliers[[1]][[2]], linetype = 2, size = 0.75, col = "dodgerblue") +
    #geom_hline(yintercept = s_outliers[[2]][[2]], linetype = 2, size = 0.75, col = "navyblue") +
    #geom_hline(yintercept = s_outliers[[3]][[2]], linetype = 2, size = 0.75, col = "purple") +
    theme( axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(title = "Sample total peak area (TPA) by BOX ID", 
         subtitle = paste0( "   ---  " ,  
                            eta ,"% of the varition in sample TPA can be explained by BOX ID (pval = ", 
                            pval, ").\n   ---  TPA estiamted with complete features only  ---"),
         y = "TPA", x = "Box ID", color = "Box ID")
  
  ###########################
  ## Run day on TPA (complete features)
  ###########################
  fit = lm(sample_tpa_complete ~ as.factor(RUN_DAY), data = sdata)
  a = anova(fit)
  eta = signif( a[1,2]/sum(a[,2]), d = 2 )
  pval = signif( a[1, 5], d = 3 )
  ###
  plotD = sdata  %>% ggplot( aes(x = as.factor(RUN_DAY), y = sample_tpa_complete)) +
    #geom_boxplot( aes(color = as.factor(RUN_DAY) ), notch = TRUE) +
    geom_violin( aes(color = as.factor(RUN_DAY) ) ) +
    #geom_boxplot(aes(color = as.factor(RUN_DAY) ) , width=0.1) +
    geom_dotplot( aes(color = as.factor(RUN_DAY)), 
                  binaxis='y', stackdir='center', 
                  dotsize=0.35, alpha= 0.6) +
    geom_hline(yintercept = s_outliers[[1]][[3]], linetype = 2, size = 0.75, col = "orange") +
    geom_hline(yintercept = s_outliers[[2]][[3]], linetype = 2, size = 0.75, col = "orange3") +
    geom_hline(yintercept = s_outliers[[3]][[3]], linetype = 2, size = 0.75, col = "red") +
    geom_hline(yintercept = s_outliers[[1]][[2]], linetype = 2, size = 0.75, col = "dodgerblue") +
    #geom_hline(yintercept = s_outliers[[2]][[2]], linetype = 2, size = 0.75, col = "navyblue") +
    #geom_hline(yintercept = s_outliers[[3]][[2]], linetype = 2, size = 0.75, col = "purple") +
    theme( axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(title = "Sample total peak area (TPA) by RUN DAY", 
         subtitle = paste0( "   ---  " , 
                            eta ,"% of the varition in sample TPA can be explained by RUN DAY (pval = ", 
                            pval, ").\n   ---  TPA estiamted with complete features only  ---"),
         y = "TPA", x = "Run Day", color = "Run Day")
  
  toplot = list( tpa_tpa_plot = plotA, mis_tpa_plot = plotB, boxid_plot = plotC, runday_plot = plotD)
  return(toplot)
  }