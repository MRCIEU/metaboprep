#' @title Identify Independent Features in a Numeric Matrix
#' @description
#' This function identifies independent features using Spearman's rho correlation distances, and a dendrogram
#' tree cut step.
#' @param data matrix, the metabolite data matrix. samples in row, metabolites in columns
#' @param tree_cut_height the tree cut height. A value of 0.2 (1-Spearman's rho) is equivalent to saying that features with a rho >= 0.8 are NOT independent.
#' @param features_exclude character, vector of feature id indicating features to exclude from the sample and PCA summary analysis but keep in the data
#' 
#' @keywords independent features
#'
#' @return a list object of (1) an hclust object, (2) independent features, (3) a data frame of feature ids, k-cluster identifiers, and a binary identifier of independent features
#' 
#' @importFrom stats cor as.dist hclust cutree
#' 
#' @export
#'
tree_and_independent_features = function(data, tree_cut_height = 0.5, features_exclude = NULL){

  # testing
  if (FALSE) {
    tree_cut_height = 0.5
    features_exclude = NULL
    set.seed(123)
    data <- matrix(rnorm(1000), nrow = 100, ncol = 10)
    colnames(data) <- paste0("F", 1:10)
    data[sample(length(data), 100)] <- NA
    data <- cbind(data, F_dup = data[, "F2"] + rnorm(100, sd = 0.01))
  }
  
  
  # remove excluded features
  if (!is.null(features_exclude)) {
    data <- data[, !colnames(data) %in% features_exclude]
  }
  
  
  # remove features with no variance
  row_var0 <- which( apply(data, 2, function(x) var(x,na.rm=T)==0) )
  if(length(row_var0) > 0){
    data <- data[, -row_var0]
  }
  
  
  # remove features with <80% presence 
  low_presence <- which(colMeans(!is.na(data)) < 0.8)
  if(length(low_presence) > 0){
    data <- data[, -low_presence]
  }
  
  
  # make tree
  cor_matrix  <- stats::cor(data, method="spearman", use = "pairwise.complete.obs")
  dist_matrix <- stats::as.dist(1-abs(cor_matrix))
  stree       <- stats::hclust(dist_matrix, method = "complete")

  
  # restrict based on cut off
  k <- stats::cutree(stree, h = tree_cut_height)
  k_group <- table(k)
  

  # keep all single-feature clusters
  ind_k <- names(k_group[k_group == 1])
  ind   <- names(k)[k %in% ind_k]
  
  
  # clusters with >1 feature, pick one with highest variance explained
  cluster_ids <- names(k_group[k_group > 1])
  ind2 <- sapply(cluster_ids, function(cluster) {
    members <- names(k)[k == as.integer(cluster)]
    sub_cor <- cor_matrix[members, members]
    cor_sums <- rowSums(abs(sub_cor), na.rm = TRUE)
    best_feature <- names(which.max(cor_sums))
    return(best_feature)
  })
  
  independent_features <- paste( c(ind, ind2) )


  # return 
  out <- data.frame("feature_id"           = colnames(data),
                    "k"                    = k[match(colnames(data), names(k))],
                    "independent_features" = colnames(data) %in% independent_features)
  
  return(list(data = out, tree = stree))
}


