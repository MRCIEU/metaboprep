#' A Function estiamtes total peak area for each sample of a metabolite matrix
#'
#' This function estimates total peak area for all features and all features with complete data. 
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param feature_names_2_exclude A vector of feature|metabolite names to exclude from the tree building, independent feature identification process.
#' @keywords metabolomics
#' @export
#' @examples
#' total.peak.area()
total.peak.area <- function(wdata, feature_names_2_exclude = NA){
  if( !is.na(feature_names_2_exclude[1]) ){
    r = which(colnames(wdata) %in% feature_names_2_exclude)
    if(length(r)>0){
      wdata = wdata[,-r]  
    }
  }
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




