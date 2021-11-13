#' generate a hclust dendrogram
#'
#' This estimates a dendrogram of feautres based on correlation coefficeint of your choice, and a clustering method of choice
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param cor_method the correlation method used in the function cor(). Default is "spearman".
#' @param hclust_method the dendrogram clustering method used in the construction of the tree. Default is "complete".
#'
#' @keywords hclust dendrogram
#' 
#' @importFrom stats cor as.dist hclust
#' 
#' @return an hclust object
#'
#' @export 
#'
#' @examples
#' cmat = matrix(1, 4, 4 )
#' cmat[1,] = c(1, 0.8, 0.6, 0.2)
#' cmat[2,] = c(0.8, 1, 0.7, 0.5)
#' cmat[3,] = c(0.6, 0.7, 1, 0.6)
#' cmat[4,] = c(0.2, 0.5, 0.6,1)
#' ## simulate some correlated data (multivariable random normal)
#' set.seed(1110)
#' ex_data = MASS::mvrnorm(n = 250, mu = c(5, 45, 25, 15), Sigma = cmat )
#' ## estiamte the dendrogram
#' tree = make.tree(ex_data)
#' ## plot the dendrogram
#' plot(tree, hang = -1)
#'
make.tree = function(wdata, cor_method = "spearman", hclust_method = "complete"){
  
  cat(paste0("\t\t\t- Generating Correlation Matrix.\n"))
  cor_matrix <- stats::cor(wdata, method=cor_method, use = "pairwise.complete.obs")
  #cor_matrix <- rcorr(wdata, type=cor_method)


  cat(paste0("\t\t\t- Generating Distance Matrix.\n"))
  dist_matrix <- stats::as.dist(1-abs(cor_matrix))

  cat(paste0("\t\t\t- Constructing dendrogram.\n"))
  tree <- stats::hclust(dist_matrix, method = hclust_method)
  return(tree)
}




