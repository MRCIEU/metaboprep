#' @title Identify Independent Features in a Numeric Matrix
#' @description
#' This function identifies independent features using Spearman's rho correlation distances, and a dendrogram
#' tree cut step.
#' @param data matrix, the metabolite data matrix. samples in row, metabolites in columns
#' @param tree_cut_height the tree cut height. A value of 0.2 (1-Spearman's rho) is equivalent to saying that features with a rho >= 0.8 are NOT independent.
#'
#' @keywords independent features
#'
#' @return a list object of (1) an hclust object, (2) independent features, (3) a data frame of feature ids, k-cluster identifiers, and a binary identifier of independent features
#' 
#' @importFrom stats cor as.dist hclust cutree
#' 
#' @export
#'
tree_and_independent_features = function(data, tree_cut_height = 0.5){

  # identify features with no variance
  row_var0 <- which( apply(data, 2, function(x) var(x,na.rm=T)==0) )
  if(length(row_var0) > 0){
    data <- data[, -row_var0]
  }

  
  # make tree
  cor_matrix  <- stats::cor(data, method="spearman", use = "pairwise.complete.obs")
  dist_matrix <- stats::as.dist(1-abs(cor_matrix))
  stree       <- stats::hclust(dist_matrix, method = "complete")

  
  # restrict based on cut off
  k <- stats::cutree(stree, h = tree_cut_height)

  
  # restrict so as to keep the feature with the least missingness
  k_group = table(k)
  

  # strictly independent features
  w     <- which(k_group == 1)
  ind_k <- names( k_group[w] )
  w     <- which(k %in% ind_k)
  ind   <- names(k)[w]

  
  # identify feature with least missingness in each group
  N       <- apply(data, 2, function(x){ sum(!is.na(x)) })
  w       <- which(k_group > 1) 
  k_group <- names( k_group[w] )
  
  ind2 <- sapply(k_group, function(x){
    w   <- which(k %in% x); n = names( k[w] )
    o   <- sort(N[n], decreasing = FALSE)
    out <- names(o)[1]
    return(out)
  })
  independent_features <- paste( c(ind, ind2) )


  # return 
  out <- data.frame("feature_id"           = colnames(data),
                    "k"                    = k[match(colnames(data), names(k))],
                    "independent_features" = colnames(data) %in% independent_features)

  # and tree as attribute for later 
  attr(out, "")
  
  return(list(data = out, tree = stree))
}


