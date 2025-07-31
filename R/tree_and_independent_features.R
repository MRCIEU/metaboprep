#' @title Identify Independent Features in a Numeric Matrix
#' @description
#' This function identifies independent features using Spearman's rho correlation distances, and a dendrogram
#' tree cut step.
#' @param data matrix, the metabolite data matrix. samples in row, metabolites in columns
#' @param tree_cut_height the tree cut height. A value of 0.2 (1-Spearman's rho) is equivalent to saying that features with a rho >= 0.8 are NOT independent.
#' @param features_exclude character, vector of feature id indicating features to exclude from the sample and PCA summary analysis but keep in the data
#' @param feature_selection character. Method for selecting a representative feature from each correlated feature cluster. 
#' One of:
#' \describe{
#'   \item{\code{"max_var_exp"}}{(Default) Selects the feature with the highest sum of absolute Spearman correlations to other features in the cluster; 
#'   effectively the feature explaining the most shared variance.}
#'   \item{\code{"least_missingness"}}{Selects the feature with the fewest missing values within the cluster.}
#' }
#' @return A list with the following components:
#' \describe{
#'   \item{data}{A `data.frame` with:
#'     \itemize{
#'       \item `feature_id`: Feature (column) names from the input matrix.
#'       \item `k`: The cluster index assigned to each feature after tree cutting.
#'       \item `independent_features`: Logical indicator of whether the feature was selected as an independent (representative) feature.
#'     }
#'   }
#'   \item{tree}{A `hclust` object representing the hierarchical clustering of the features based on 1 - |Spearman's rho| distance.}
#' }
#' 
#' @importFrom stats cor as.dist hclust cutree
#' 
#' @export
#'
tree_and_independent_features = function(data, tree_cut_height = 0.5, features_exclude = NULL, feature_selection = "max_var_exp"){

  # testing
  if (FALSE) {
    tree_cut_height = 0.5
    features_exclude = NULL
    feature_selection <- "least_missingness"
    set.seed(123)
    data <- matrix(rnorm(1000), nrow = 100, ncol = 10)
    colnames(data) <- paste0("F", 1:10)
    data[sample(length(data), 100)] <- NA
    data <- cbind(data, F_dup = data[, "F2"] + rnorm(100, sd = 0.01))
  }
  
  
  # checks 
  feature_selection <- match.arg(feature_selection, choices = c("max_var_exp", "least_missingness"))
  
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
  
  
  # pick 1 feature within the clusters of size > 1
  cluster_ids <- names(k_group[k_group > 1])
  
  if (feature_selection == "least_missingness") {
    
    N       <- apply(data, 2, function(x){ sum(!is.na(x)) })
    ind2 <- sapply(cluster_ids, function(x){
      w   <- which(k %in% x)
      n   <- names( k[w] )
      o   <- sort(N[n], decreasing = FALSE)
      out <- names(o)[1]
      return(out)
    })
    
  } else if (feature_selection == "max_var_exp") {
    
    # clusters with >1 feature, pick one with highest variance explained
    ind2 <- sapply(cluster_ids, function(cluster) {
      members      <- names(k)[k == as.integer(cluster)]
      sub_cor      <- cor_matrix[members, members]
      cor_sums     <- rowSums(abs(sub_cor), na.rm = TRUE)
      best_feature <- names(which.max(cor_sums))
      return(best_feature)
    })
    
  } else {
    
    stop("feature_selection parameter not recognised - (this shouldn't happen)")
    
  }

  
  independent_features <- paste( c(ind, ind2) )


  # return 
  out <- data.frame("feature_id"           = colnames(data),
                    "k"                    = k[match(colnames(data), names(k))],
                    "independent_features" = colnames(data) %in% independent_features)
  
  return(list(data = out, tree = stree))
}


