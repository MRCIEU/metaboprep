#' @title Estimate Missingness
#' @description
#' This function estimates missingness in a matrix of data and provides an option to exclude certain columns or features from the analysis, such as xenobiotics (with high missingness rates) in metabolomics data sets.
#' @param data matrix, a numeric matrix with samples in rows and features in columns
#' @param by character, whether to calculate missingness by rows (samples) or column (features)
#' @return data.frame, a data frame of missingness estimates for each sample/feature.
#'
#' @export
#'
missingness <- function(data, by = "row") {

  by <- match.arg(by, choices = c("row", "column"))

  if (by == "row") {
    margin <- 1
    id_col <- "sample_id"
  } else if (by == "column") {
    margin <- 2
    id_col <- "feature_id"
  }

  miss <- apply(data, margin, function(x) { sum(is.na(x)) / length(x) })
  out <- data.frame("ids"       = names(miss),
                    missingness = miss)

  # if( !all(is.na(exclude_features)) || length(exclude_features)>0 ) {
  # 
  #   r = which(colnames(data) %in% exclude_features)
  # 
  #   if (length(r) > 0) {
  # 
  #     miss2 <- apply(data[, -r, drop = FALSE], margin, function(x) { sum(is.na(x)) / length(x) })
  # 
  #     out2  <- data.table::data.table(ids = names(miss2),
  #                                     missingness_w_exclusions = miss2)
  # 
  #     out[out2, missingness_w_exclusions := i.missingness_w_exclusions, on="ids"]
  # 
  #   }

  # }

  names(out)[names(out) == "ids"] <- id_col

  return(out)
}
