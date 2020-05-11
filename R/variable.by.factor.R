#' A Function to perform univariate analysis over a dependent and an independent variable
#'
#' This function performs univariate analysis over a dependent and an independent variable
#' @param dep a vector of the dependent variable
#' @param indep a vector of the independent variable
#' @param dep_name name of the dependent variable
#' @param indep_name name of the independent variable
#' @param orderfactor order factors alphebetically
#' @param violin box plot or violin plot. violin = TRUE is default
#' @keywords metabolomics
#' @export
#' @examples
#' variable.by.factor()
variable.by.factor = function( dep , indep , 
                               dep_name = "dependent", 
                               indep_name = "independent", 
                               orderfactor = TRUE,
                               violin = TRUE){
  ####
  wdat = as_tibble( data.frame(dep = dep, indep = as.factor(indep)) )
  ####
  if(orderfactor == 1){
    ord = wdat %>% group_by(indep) %>%
      summarise( m =  mean(dep),
                 CI_L = quantile(dep, 0.025),
                 CI_H = quantile(dep, 0.975)) %>%
      arrange(m)
    ##### order factor
    wdat$indep = factor(wdat$indep, levels = ord$indep, ordered = TRUE)
  }
  
  ### FIT to linear MODEL
  fit = lm(dep ~ indep, data = wdat)
  a = anova(fit)
  eta = signif( (a[1,2]/sum(a[,2])*100 ), d = 2 )
  pval = signif( a[1, 5], d = 3 )
  ###
  if(violin == 1){
  plotA = wdat  %>% ggplot( aes(x = indep, y = dep)) +
    geom_violin( aes(fill = as.factor(indep) ), color = NA ) +
    #geom_dotplot(aes(color = as.factor(indep) ), 
    geom_dotplot(fill = "grey" , color = NA, 
                 binaxis='y', stackdir='center', 
                 dotsize=0.35, alpha= 1,
                 binwidth = 0.006) +
    theme( axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(title = paste0( dep_name, " by ", indep_name), 
         subtitle = paste0( "   ---  " ,  
                            eta ,
                            "% of the varition in ", 
                            dep_name, " can be explained by ",
                            indep_name, "(pval = ", 
                            pval, ")."),
         y = dep_name, x = indep_name, fill = indep_name)
  } else {
    plotA = wdat  %>% ggplot( aes(x = indep, y = dep)) +
      geom_boxplot( aes(fill = as.factor(indep)), notch = FALSE) +
      theme( axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
      labs(title = paste0( dep_name, " by ", indep_name), 
           subtitle = paste0( "   ---  " ,  
                              eta ,
                              "% of the varition in ", 
                              dep_name, " can be explained by ",
                              indep_name, "(pval = ", 
                              pval, ")."),
           y = dep_name, x = indep_name, fill = indep_name)
    }
  return(plotA)
  
  
  }

