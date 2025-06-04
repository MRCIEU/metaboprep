#' @title Estimates total peak abundance
#' @description
#' This function estimates total peak abundance|area for numeric data in a matrix, for (1) all features and (2) all features with complete data.
#' @param data matrix, the metabolite data matrix. Samples in rows, metabolites in columns
#' @param ztransform logical, should the feature data be z-transformed and absolute value minimum, mean shifted prior to summing the feature values. TRUE or FALSE.
#'
#' @return a data frame of estimates for (1) total peak abundance and (2) total peak abundance at complete features for each samples
#'
#' @export
#'
total_peak_area <- function(data, ztransform = TRUE){

  # z-transformed data frame
  if(ztransform == TRUE){
    data = apply(data, 2, function(x) {
      ( x - mean(x, na.rm = TRUE) ) / sd(x, na.rm = TRUE)
    })
    ## add absolute(minimum) value to all values
    # cat(paste0("\t\t\t- adding absolute minimum observed value to all values to make all values positive.\n") )
    data = data + abs(min(data, na.rm = TRUE))
  }

  # total peak area
  total_tpa = apply(data, 1, function(x){ sum(x, na.rm = TRUE) })
  
  # find features with complete data
  mis = apply(data, 2, function(x){ sum(is.na(x))/length(x) })
  completefeatures = which(mis == 0)
  
  # TPA with complete features
  completeF_tpa = apply(data[, completefeatures], 1, function(x){ sum(x, na.rm = TRUE) })

  # output data.frame
  out = data.frame("sample_id" = names(total_tpa), "tpa_total" = total_tpa, "tpa_complete_features" = completeF_tpa)
  
  # return result
  return(out)
}
