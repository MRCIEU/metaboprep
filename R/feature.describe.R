#' summary statistics for features 
#'
#' This function allows you to 'describe' metabolite features using the describe() function from the psych package, as well as estimate variance, a dispersion index, the coeficent of variation, and shapiro's W-statistic.
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#'
#' @keywords feature summary statistics
#'
#' @importFrom psych describe
#' @importFrom stats var sd na.omit shapiro.test
#' 
#' @return a data frame of summary statistics for features (columns) of a matrix
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:20, function(x){ rnorm(250, 40, 5) })
#' feature.describe(ex_data)
#'
feature.describe = function(wdata){
  ## package check
  pkgs = c("psych")
  for(pkg in pkgs){
    if (!requireNamespace( pkg, quietly = TRUE)) {
        stop(paste0("Package \"", pkg,"\" needed for feature.describe() function to work. Please install it."),call. = FALSE)
      }
  }

  d = psych::describe(wdata)[, -c(1,6,7)]
  ####
  e = t( apply(wdata, 2, function(x){
    ## missing
    missing_count = sum(is.na(x))

    ## variance
    v = stats::var(x, na.rm = TRUE); if(is.na(v)){v = 0}
    
    ## dispersion index
    dispersionindex = v / mean(x, na.rm = T)
    if(v == 0){dispersionindex = NA}
    
    ## coef of var
    coefvar = stats::sd(x, na.rm = T) / mean(x, na.rm = T)
    
    ## shapiro test for normality
    ## it can only have 5000 observation
    if(length(x) > 5000 ){
      x = sample(x, 5000, replace = FALSE)
    }
    if(v == 0 | length(stats::na.omit(x)) < 5 ){
        W_raw = NA; W_log = NA
      } else {
      W_raw = stats::shapiro.test(x)$stat
      
      ## normalized data may have negative values
      ## we must account for this possibility
      ## before log transformation
      a = x
      if( sum(a<0,na.rm = TRUE)>0 ){ a = a-min(a, na.rm = TRUE) }

      ## if there are any 0 values in the data
      ## we shall set the to 1/2 of the obs min
      if( sum(a==0,na.rm = TRUE)>0 ){ 
        w = which(a==0)
        a[w] = min(a[-w], na.rm = TRUE)/2  
       }
      ## log transform    
      a = log10(a)
      ## shapiro test of log data
      W_log = shapiro.test(  a  )$stat
      }
    
    ## data to return
    o = c(missing_count, v, dispersionindex, coefvar, W_raw, W_log)
    names(o) = c("missing","var","disp_index","coef_variance","W","log10_W")
    return(o)

    }) )

  out = cbind(d, e)
  return(out)
  
  }



