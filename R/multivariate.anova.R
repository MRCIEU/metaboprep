#' multivariate analysis
#'
#' This function performs a multivariate analysis over a dependent|response and numerous independent|explanatory variable
#'
#' @param dep a vector of the dependent variable
#' @param indep_df a data frame of the independent variable
#'
#' @keywords metabolomics multivariate ANOVA
#'
#' @importFrom stats lm
#' @importFrom car Anova
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select everything
#' @importFrom ggpubr ggtexttable ttheme
#' @importFrom magrittr %>%
#' 
#' @return ggplot2 table figure of
#'
#' @export
#'
#' @examples
#' cmat = matrix(1, 3, 3 )
#' cmat[1,] = c(1, 0.5, 0.3)
#' cmat[2,] = c(0.5, 1, 0.25)
#' cmat[3,] = c(0.3, 0.25, 1)
#' ## simulate some correlated data (multivariable random normal)
#' set.seed(1110)
#' ex_data = MASS::mvrnorm(n = 250, mu = c(5, 45, 25), Sigma = cmat )
#' colnames(ex_data) = c("outcome","age","bmi")
#' multivariate.anova(dep = ex_data[,1], indep_df = ex_data[, 2:3])
#'
multivariate.anova = function(dep, indep_df){
  ## define local variable
  batch.variable <- NULL
  
  ## package check
  pkgs = c("stats", "car", "tibble", "dplyr", "ggpubr", "magrittr")
  for(pkg in pkgs){
    if (!requireNamespace( pkg, quietly = TRUE)) {
        stop(paste0("Package \"", pkg,"\" needed for multivariate.anova() function to work. Please install it."),call. = FALSE)
      }
  }
  
  wdat = data.frame( cbind(indep_df, dep) )
  ## fit the model
  fit = lm(dep ~ . , data = wdat)
  
  
  ## Type I ANOVA
  a = car::Anova(fit, type = "II")
  eta = round( a[ ,1] / sum(a[,1], na.rm = TRUE), digits = 4 )*100
  names(eta) = paste0( rownames(a) ,"_eta")
  pval = c( formatC( a[-nrow(a), 4], format = "e", digits = 4) , "NA")
  names(pval) = paste0( rownames(a) ,"_pval")
  
  ## Make an reporting table
  outtable = tibble::as_tibble( matrix( c(eta, pval), ncol = 2, byrow = FALSE, 
                                dimnames = list( c(rownames(a)), 
                                                 c("etasq.var.exp","pvalue")  ) ) )
  ##
  outtable = outtable %>% mutate( batch.variable = rownames(a) ) %>% dplyr::select( batch.variable, everything())
  ##
  outtable[,1] = tolower( unlist( outtable[,1]) )
  outtable[,1] = gsub("_",".", unlist(outtable[,1]) )
  
  ## make the table a ggplot like figure
  outtable <- ggpubr::ggtexttable(outtable, rows = NULL, 
                          theme = ggpubr::ttheme("mBlue"))
  ## return output
  return(outtable)
}

