#' @title Principal Component Analysis
#' @description
#' This function performs two principal component analysis. In the first, missing data is imputed to the median. In the second a probablistic PCA is run to account for the missingness.
#' Subsequent to the derivation of the PC, the median imputed PC data is used to identify the number of informative or "significant" PC by (1) an acceleration analysis, and (2) a parrallel analysis.
#' Finally the number of sample outliers are determined at 3, 4, and 5 standard deviations from the mean on the top PCs as determined by the acceleration factor analysis.
#'
#' @param metaboprep an object of class Metaboprep
#' @param source_layer character, type/source of data to use
#' @param sample_ids character, vector of sample ids to include, default NULL includes all
#' @param feature_ids character, vector of feature ids to include, default NULL includes all
#' 
#' @importFrom stats prcomp
#' @importFrom pcaMethods ppca
#' @importFrom nFactors parallel nScree
#'
#' @return a data.frame
#' @export
#'
pc_and_outliers <- new_generic("pc_and_outliers", c("metaboprep"), function(metaboprep, source_layer="input", sample_ids=NULL, feature_ids=NULL) { S7_dispatch() })
#' @name pc_and_outliers
method(pc_and_outliers, Metaboprep) <- function(metaboprep, source_layer="input", sample_ids=NULL, feature_ids=NULL) {

  # options & checks
  source_layer <- match.arg(source_layer, choices = dimnames(metaboprep@data)[[3]])
  stopifnot("sample_ids must all be found in the data" = all(sample_ids %in% metaboprep@samples[["sample_id"]]) | is.null(sample_ids))
  stopifnot("feature_ids must all be found in the data" = all(feature_ids %in% metaboprep@features[["feature_id"]]) | is.null(feature_ids))  
  
  
  # get ids
  if (is.null(sample_ids)) sample_ids   <- metaboprep@samples[["sample_id"]]
  if (is.null(feature_ids)) feature_ids <- metaboprep@features[["feature_id"]]
  
  
  # set data
  pcadata <- metaboprep@data[sample_ids, feature_ids, source_layer]
  
  
  # impute missingness as medians
  pcadata = median_impute(data = pcadata)

  
  # z-transformation
  pcadata = apply(pcadata, 2, function(x){
    ( x - mean(x, na.rm = TRUE) ) / sd(x, na.rm = TRUE)
  })
  
  
  # perform PCA
  mypca  <- stats::prcomp(pcadata, center = FALSE, scale = FALSE)
  varexp <- summary(mypca)[[6]][2, ]

  
  # find number of sig PCs
  ev <- eigen(cor(pcadata))
  ap <- nFactors::parallel(subject=nrow(pcadata), var=ncol(pcadata), rep=100, cent=.05)
  ns <- nFactors::nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  af <- as.numeric( ns[[1]][["naf"]] )
  if(af < 2) { af = 2 }
  nsig_parrallel <- ns[[1]][["nparallel"]]


  # identify outliers - 3SD
  o_mat3           <- outlier_detection(mypca$x[, 1:af], nsd = 3, by = "column")
  colnames(o_mat3) <- paste0(colnames(o_mat3), "_3_sd_outlier")
  pc_out           <- cbind(mypca$x[,1:10], o_mat3)
   
  
  # identify outliers - 4SD
  o_mat4           <- outlier_detection(mypca$x[, 1:af], nsd = 4, by = "column")
  colnames(o_mat4) <- paste0(colnames(o_mat4), "_4_sd_outlier")
  pc_out           <- cbind(pc_out, o_mat4)
 
  
  # identify outliers - 5SD
  o_mat5           <- outlier_detection(mypca$x[, 1:af], nsd = 5, by = "column")
  colnames(o_mat5) <- paste0(colnames(o_mat5), "_5_sd_outlier")
  pc_out           <- cbind(pc_out, o_mat5)
  colnames(pc_out) <- tolower(colnames(pc_out))


  # set attributes with processing details
  pc_out <- as.data.frame(pc_out)
  pc_out <- cbind(sample_id = rownames(pc_out), pc_out)
  attr(pc_out, "varexp")           <- varexp
  attr(pc_out, "num_pcs_scree")    <- af
  attr(pc_out, "num_pcs_parallel") <- nsig_parrallel
  
  
  # sample_id as data.frame
  return(pc_out)
}


median_impute = function( data ){
  rn = rownames(data)
  cn = colnames(data)
  out = sapply( 1:ncol(data), function(i){
    x = as.numeric( data[, i]  )
    m = median(x, na.rm = TRUE)
    w = which(is.na(x))
    x[w] = m
    return( t(x) )
  })
  rownames(out) = rn
  colnames(out) = cn
  return(out)
}


