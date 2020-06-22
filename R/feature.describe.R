#' A Function to describe each feature of a matrix of metabolite data. 
#'
#' This function allows you to 'describe' metabolite features using the describe() function from the psych package, as well as estimate variance, a dispersion index, the coeficent of variation, and shapiro's W-statistic.
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @keywords metabolomics
#' @export
#' @examples
#' feature.describe()
feature.describe = function(wdata){
  d = psych::describe(wdata)[, -c(1,6,7)]
  ####
  e = t( apply(wdata, 2, function(x){
    v = var(x, na.rm = TRUE); if(is.na(v)){v = 0}
    
    ## dispersion index
    dispersionindex = v / mean(x, na.rm = T)
    if(v == 0){dispersionindex = NA}
    
    ## coef of var
    coefvar = sd(x, na.rm = T) / mean(x, na.rm = T)
    
    ## shapiro test for normality
    ## it can only have 5000 observation
    if(length(x) > 5000 ){
      x = sample(x, 5000, replace = FALSE)
    }
    if(v == 0 | length(na.omit(x)) < 5 ){
        W_raw = NA; W_log = NA
      } else {
      W_raw = shapiro.test(x)$stat
      ## accounting for 0's
      # a = x[x != 0]
      a = log10(x)
      w = which(a == "-Inf")
      a[w] = min(a[-w], na.rm = TRUE)
      W_log = shapiro.test(  a  )$stat
      }
    
    ## data to return
    o = c(v, dispersionindex, coefvar, W_raw, W_log)
    names(o) = c("var","disp_index","coef_variance","W_stat_rawdata","W_stat_log10data")
    return(o)

    }) )

  out = cbind(d, e)
  return(out)
  
  }



