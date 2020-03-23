#' A Function estiamtes total peak area for each sample of a metabolite matrix
#'
#' This function estimates total peak area for all features and all features with complete data. 
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @keywords metabolomics
#' @export
#' @examples
#' total.peak.area()
total.peak.area <- function(wdata){
  ## total peak area
  total_tpa = apply(wdata, 1, function(x){ sum(x, na.rm = TRUE) })
  ### find features with complete data
  mis = apply(wdata, 2, function(x){ sum(is.na(x))/length(x) })
  completefeatures = which(mis == 0)
  ## TPA with complete features
  completeF_tpa = apply(wdata[, completefeatures], 1, function(x){ sum(x, na.rm = TRUE) })
  ##
  out = data.frame(TPA_total = total_tpa, TPA_completefeature = completeF_tpa)
  return(out)
}




