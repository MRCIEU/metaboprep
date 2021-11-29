#' feature summary statistics
#'
#' This function estimates feature statistics for samples in a matrix of metabolite features. 
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param sammis a vector of sample missingness estimates, that is ordered to match the samples in the rows of your data matrix.
#' @param tree_cut_height tree cut height is the height at which to cut the feature|metabolite dendrogram to identify "independent" features. tree_cut_height is 1-absolute(Spearman's Rho) for intra-cluster correlations.
#' @param outlier_udist the interquartile range unit distance from the median to call a sample an outlier at a feature.
#' @param feature_names_2_exclude A vector of feature|metabolite names to exclude from the tree building, independent feature identification process. 
#'
#' @keywords metabolomics
#' 
#' @return a list object of length two, with (1) a data frame of summary statistics and (2) a hclust object
#'
#' @export
#'
#' @examples
#' ## define a covariance matrix
#' cmat = matrix(1, 4, 4 )
#' cmat[1,] = c(1, 0.8, 0.6, 0.2)
#' cmat[2,] = c(0.8, 1, 0.7, 0.5)
#' cmat[3,] = c(0.6, 0.7, 1, 0.6)
#' cmat[4,] = c(0.2, 0.5, 0.6,1)
#' ## simulate some correlated data (multivariable random normal)
#' set.seed(1110)
#' d1 = MASS::mvrnorm(n = 250, mu = c(5, 45, 25, 15), Sigma = cmat )
#' set.seed(1010)
#' d2 = MASS::mvrnorm(n = 250, mu = c(5, 45, 25, 15), Sigma = cmat )
#' ## simulate some random data
#' d3 = sapply(1:20, function(x){ rnorm(250, 40, 5) })
#' ## define the data set
#' ex_data = cbind(d1,d2,d3)
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## add in some missingness
#' ex_data[sample(1:length(ex_data), 450)] = NA
#' ## add in some technical error to two samples
#' m = apply(ex_data, 2, function(x){ mean(x, na.rm = TRUE) })
#' ex_data[c(1,10), ] = ex_data[1, ] + (m*0.00001) 
#' ## run the function
#' fss = feature.sum.stats(ex_data)
#' ## feature summary table
#' fss$table[1:5, ]
#' ## plot the dendrogram
#' plot(fss$tree, hang = -1)
#'
feature.sum.stats = function( wdata, sammis = NA, tree_cut_height = 0.5, outlier_udist = 5, feature_names_2_exclude = NA){
  ## feature missingness
  featuremis = feature.missingness(wdata)
  ### distribution discritions
  description = feature.describe(wdata)
  ### count of sample outliers per feature
  foutlier = feature.outliers(wdata, nsd = outlier_udist)
  ### identify independent features
  # if(50 > nrow(wdata)*0.8 ){
  #   MSS = nrow(wdata) * 0.8  ## this allows 20% missingness on data sets with less than 50 individuals
  # } else {
  #   MSS = 50  
  # }
  ## ** ALL FEATURES THAT DO INTO THE DENDROGRAM AND CAN BE REPRESENTITIVE
  ##    FEATURES MUST HAVE > 80% Presence or <= 20% missing
  MSS = floor( nrow(wdata) * 0.8 )
  ##
  indf = tree_and_independent_features(wdata, minimum_samplesize = MSS, tree_cut_height = tree_cut_height, feature_names_2_exclude = feature_names_2_exclude )
  ### data out
  out = cbind(featuremis, foutlier, description, indf[[3]][, -1] )
  ###
  feature_name = rownames(out)
  out = cbind(feature_name, out)
  ###
  return(  list( table = out, tree = indf[[1]] ) )
}



