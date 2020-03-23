#' A Function to estimate numerous feature summary statistics for your samples
#'
#' This function estiamtes feature statistics for samples in a matrix of metabolite features. 
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param sammis a vector of sample missingness estimates, that is ordered to match the samples in the rows of your data matrix.
#' @keywords metabolomics
#' @export
#' @examples
#' feature.sum.stats()
feature.sum.stats = function( wdata, sammis = NA){
  ## feature missingness
  featuremis = feature.missingness(wdata, samplemissingness = sammis)
  ### distribution discritions
  description = feature.describe(wdata)
  ### count of sample outliers per feature
  foutlier = feature.outliers(wdata)
  ### identify independent features
      #indf = feature.tree.independence(wdata)
  if(50 < nrow(wdata)*0.8 ){
    MSS = nrow(wdata) * 0.8
  } else {
    MSS = 50  
  }
  indf = tree_and_independent_features(wdata, minimum_samplesize = MSS, treecutheight = 0.4)
  ### data out
  out = cbind(featuremis, foutlier, description, indf[[3]] )
  return(  list( table = out, tree = indf[[1]] ) )
}



