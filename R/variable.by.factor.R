#' ggplot2 violin plot
#'
#' This function performs univariate linear analysis of a dependent and an independent variable and generates a viloin or box plot to illustrate the associated structure.
#'
#' @param dep a vector of the dependent variable
#' @param indep a vector of the independent variable
#' @param dep_name name of the dependent variable
#' @param indep_name name of the independent variable
#' @param orderfactor order factors alphebetically
#' @param violin box plot or violin plot. violin = TRUE is default
#'
#' @keywords metabolomics ggplot
#'
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise arrange
#' @importFrom ggplot2 aes geom_violin geom_boxplot theme labs
#' 
#' @return a ggplot2 object
#'
#' @export
#'
#' @examples
#' x = c( rnorm(20, 10, 2), rnorm(20, 20, 2) )
#' y = as.factor( c( rep("A", 20), rep("B", 20)  ) )
#' variable.by.factor(dep = x , indep = y, dep_name = "expression", indep_name = "species" )
#'
variable.by.factor = function( dep , indep , 
                               dep_name = "dependent", 
                               indep_name = "independent", 
                               orderfactor = TRUE,
                               violin = TRUE){
  ## define local variables
  m <- CI_L <- CI_H <- NULL
  ## package check
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for variable.by.factor() function to work. Please install it.",
      call. = FALSE)
  }
  ## package check
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package \"tibble\" needed for variable.by.factor() function to work. Please install it.",
      call. = FALSE)
  }

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
  eta = signif( (a[1,2]/sum(a[,2])*100 ), digits = 2 )
  pval = signif( a[1, 5], digits = 3 )
  ###
  if(violin == 1){
  plotA = wdat  %>% ggplot( aes(x = indep, y = dep)) +
    geom_violin( aes(fill = as.factor(indep) ), color = NA ) +
    theme( axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(title = paste0( dep_name, " by ", indep_name), 
         subtitle = paste0( "   ---  " ,  
                            eta ,
                            "% of the variation in ", 
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
                              "% of the variation in ", 
                              dep_name, " can be explained by ",
                              indep_name, "(pval = ", 
                              pval, ")."),
           y = dep_name, x = indep_name, fill = indep_name)
    }
  return(plotA)
  
  
  }

