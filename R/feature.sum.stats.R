#' A Function to estimate numerous feature summary statistics for your samples
#'
#' This function estiamtes feature statistics for samples in a matrix of metabolite features. 
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param sammis a vector of sample missingness estimates, that is ordered to match the samples in the rows of your data matrix.
#' @param tree_cut_height tree cut height is the height at which to cut the feature|metabolite dendrogram to identify "independent" features. tree_cut_height is 1-absolute(Spearman's Rho) for intra-cluster correlations.
#' @param feature_names_2_exclude A vector of feature|metabolite names to exclude from the tree building, independent feature identification process. 
#' @keywords metabolomics
#' @export
#' @examples
#' feature.sum.stats()
feature.sum.stats = function( wdata, sammis = NA, tree_cut_height = 0.5, feature_names_2_exclude = NA){
  ## feature missingness
  featuremis = feature.missingness(wdata, samplemissingness = sammis)
  ### distribution discritions
  description = feature.describe(wdata)
  ### count of sample outliers per feature
  foutlier = feature.outliers(wdata)
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



