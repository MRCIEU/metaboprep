#' sample exlusions on missingness and total peak area
#'
#' This function provides missingnes and tpa estiamtes along with exlcusion at 3,4,and 5sd from the mean.
#' 
#' @param mydata metabolite data
#' @param sdata sample data
#' @param fdata feature data
#'
#' @keywords metabolomics
#' 
#' @return a data frame of missingness and TPA exclusions 
#'
#' @export
#'
#' @examples
#' sam.missingness.exclusion()
#'
sam.missingness.exclusion = function(mydata, sdata, fdata){
  ## empty sample exclusion data frame
  svector = rep(0, nrow(mydata))
  sam2remove = data.frame(sample_missingness_no_xeno = svector,
                          mis20prt = svector,
                          tpa_sd3 = svector,
                          tpa_sd4 = svector,
                          tpa_sd5 = svector)
  
  ## calculate missingness after removing Xenobiotics.
  xeno = which(fdata$SUPER_PATHWAY %in% c("Xenobiotics", "xenobiotics", "xeno") )
  sam2remove$sample_missingness_no_xeno = apply(mydata[, -xeno], 1, function(x){ sum(is.na(x))/length(x) })
  
  ## identify samples with missingness greater than 20%.
  r = which( sam2remove$sample_missingness_no_xeno > 0.2)
  sam2remove$mis20prt[r] = 1
  
  ## TPA sample filtering
  ## identify outliers at SD 3, 4, and 5
  s_outliers <- lapply(3:5, function(x){outliers(sdata$sample_tpa_complete,x)})
  ##
  o3 = s_outliers[[1]][[1]]
  sam2remove$tpa_sd3[o3] = 1
  o4 = s_outliers[[2]][[1]]
  sam2remove$tpa_sd3[o4] = 1
  o5 = s_outliers[[3]][[1]]
  sam2remove$tpa_sd3[o5] = 1
  
  return(sam2remove)
  
}


