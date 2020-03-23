#' A Function to generate a dendrogram of feautres based on correlation distances
#'
#' This function allows you to generate a dendrogram of feautres based on correlation coefficeint of your choice, and a clustering method of choice
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param cor_method the correlation method used in the function cor(). Default is "spearman".
#' @param hclust_method the dendrogram clustering method used in the construction of the tree. Default is "complete"
#' @keywords metabolomics
#' @export
#' @examples
#' make.tree()
make.tree = function(wdata, cor_method = "spearman", hclust_method = "complete"){
  
  cat(paste0("\t\t\t- Generating Correlation Matrix.\n"))
  cor_matrix <- cor(wdata, method=cor_method, use = "pairwise.complete.obs")

  cat(paste0("\t\t\t- Generating Distance Matrix.\n"))
  dist_matrix <- as.dist(1-abs(cor_matrix))

  cat(paste0("\t\t\t- Constructing dendrogram.\n"))
  tree <- hclust(dist_matrix, method = hclust_method)
  return(tree)
}




