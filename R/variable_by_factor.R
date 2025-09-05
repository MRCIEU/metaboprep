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
#' @import ggplot2
#' @importFrom stats anova aggregate
#' @importFrom stringr str_wrap str_to_sentence
#'
#' @return a ggplot2 object
#'
#' @export
#'
#' @examples
#' x = c( rnorm(20, 10, 2), rnorm(20, 20, 2) )
#' y = as.factor( c( rep("A", 20), rep("B", 20)  ) )
#' variable_by_factor(dep = x , indep = y, dep_name = "expression", indep_name = "species" )
#'
variable_by_factor = function( dep , indep ,
                               dep_name = "dependent",
                               indep_name = "independent",
                               orderfactor = TRUE,
                               violin = TRUE){
  ## define local variables
  m <- CI_L <- CI_H <- NULL

  wdat = data.frame(dep = dep, indep = as.factor(indep))
  
  if(orderfactor == 1){
    ord <- aggregate(dep ~ indep, data = wdat, FUN = function(x) {
      c(m = mean(x, na.rm = TRUE),
        CI_L = quantile(x, 0.025, na.rm = TRUE),
        CI_H = quantile(x, 0.975, na.rm = TRUE))
    })
    ord_expanded <- data.frame(indep = ord$indep, ord$dep)
    
    # Order by mean
    ord_expanded <- ord_expanded[order(ord_expanded$m), ]
    
    # order factor
    wdat$indep <- factor(wdat$indep, levels = ord_expanded$indep, ordered = TRUE)
    
    #Error with ctrfn function from stats package when indep is a factor with > 95 levels, add in line to prevent this error
    stats::contrasts(wdat$indep) <- stats::contr.treatment(length(levels(wdat$indep)))
  }

  # FIT to linear MODEL ====
  fit = lm(dep ~ indep, data = wdat)
  a = stats::anova(fit)
  eta = signif( (a[1,2]/sum(a[,2])*100 ), digits = 2 )
  pval = signif( a[1, 5], digits = 3 )
  
  
  # base plot ====
  plotA <- ggplot(wdat, aes(x = indep, y = dep))
 
  
  # type ====
  if (violin == TRUE) {
    plotA <- plotA + geom_violin( aes(fill = as.factor(indep) ), color = NA )
  } else {
    plotA <- plotA + geom_boxplot( aes(fill = as.factor(indep)), notch = FALSE)
  }
  
  
  # common bits ====
  plotA <- plotA + 
    theme_minimal() + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",        # move legend below
      legend.text = element_text(size = 8), # reduce text if many levels
      legend.key.size = unit(0.4, "cm"), # smaller keys
      plot.caption = element_text(hjust = 0, margin = margin(t = 5)) # left-align caption
    ) +
    labs(subtitle = paste0(dep_name, " by ", indep_name),
         caption  = stringr::str_wrap(paste0(eta, "% of the variation in ", dep_name,
                                    " can be explained by ", indep_name,
                                    " (pval = ", pval, ")"), 
                             width = 100),
          y       = stringr::str_to_sentence(dep_name), 
          x       = indep_name, 
          fill    = indep_name) +
    guides(
      fill = "none"
    )

  
  # return ====
  return(plotA)
}

