#' estimates total peak abundance
#'
#' This function estimates total peak abundance|area for numeric data in a matrix, for (1) all features and (2) all features with complete data. 
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param feature_names_2_exclude A vector of feature|metabolite names to exclude from the tree building, independent feature identification process.
#'
#' @keywords total peak area abundance level
#' 
#' @return a data frame of estimates for (1) total peak abundance and (2) total peak abundance at complete features for each samples
#'
#' @export
#'
#' @examples
#' set.seed(1110)
#' ex_data = sapply(1:5, function(x){ rnorm(10, 40, 5) })
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' ex_data[ sample(1:50, 4) ] = NA
#' tpa_est = total.peak.area(ex_data)
#'
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




