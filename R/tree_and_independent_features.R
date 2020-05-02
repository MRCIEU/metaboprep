#' A Function to identify independent features
#'
#' This function allows you to identify independent features using Spearman's Rho, and a dendrogram tree cut step.
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param minimum_samplesize the metabolite data matrix. samples in row, metabolites in columns
#' @param tree_cut_height the tree cut height. A value of 0.2 (1-Spearman's rho) is equivalent to saying that features with a rho >= 0.8 are NOT independent. 
#' @param feature_names_2_exclude A vector of feature|metabolite names to exclude from this analysis. This might be feature heavily present|absent like Xenobiotics or variables derived from two or more variable already in the dataset.
#' @keywords metabolomics
#' @export
#' @examples
#' tree_and_independent_features()
tree_and_independent_features = function(wdata, minimum_samplesize = 50, tree_cut_height = 0.5, feature_names_2_exclude = NA){
  cat(paste0("\t\t- Estimating the number of indpendent features.\n"))
  #################
  ## feature ids
  #################
  if( !is.na(feature_names_2_exclude[1]) ){
    ## all feature IDs
    all_fids = colnames(wdata)
    ## feature ids with feature exclusions
    fids = all_fids[!all_fids %in% feature_names_2_exclude]
    ##
    wdata = wdata[, fids]
  } else {
    fids = colnames(wdata)
  }
  

  ## identify features with no variance
  rowvar0 <- which( apply(wdata,2,function(x) var(x,na.rm=T)==0) )
  if(length(rowvar0) > 0){ 
    cat(paste0("\t\t\t- ", length(rowvar0) ," features excluded from analysis for 0 variance.\n"))
    wdata = wdata[, -rowvar0] 
  }
  ## missingness filter  based on N, sample size
    #fmis = apply(wdata, 2, function(x){ sum(is.na(x))/length(x) })
    #f_remove = which(fmis > allowed_missingness )
  N = apply(wdata, 2, function(x){ sum(!is.na(x)) }) 
  f_remove = which(N < minimum_samplesize )
  if(length(f_remove) > 0){  wdata = wdata[, -f_remove] }
  ###########
  # make tree
  ###########
  stree <- make.tree(wdata=wdata, 
                     cor_method="spearman",
                     hclust_method="complete")
  
  # restrict based on cut off
  cat(paste0("\t\t\t- Performing tree cut. Cut height defined at ", tree_cut_height ,"\n"))
  k = cutree(stree, h = tree_cut_height)
  
  # restrict so as to keep the first feature in each cluster
  # independent_features <- names(k[unique(k)])
  cat(paste0("\t\t\t- Identifying independent features.\n"))
  # restrict so as to keep the feature with the least missingness
  k_group = table(k)
  ## strictly independent features
  w = which(k_group == 1); ind_k = names( k_group[w] ); w = which(k %in% ind_k); ind = names(k)[w]
  ## indentify feature with least missingness in each group
  w = which(k_group > 1); k_group =  names( k_group[w] )
  ind2 = sapply(k_group, function(x){
    w = which(k %in% x); n = names( k[w] )
    #o = sort(fmis[n], decreasing = FALSE)
    o = sort(N[n], decreasing = FALSE)
    out = names(o)[1]
    return(out)
  })
  independent_features = paste( c(ind, ind2) )
  ########################################
  ## set up returning vectors (out data)
  ########################################
  ## if feautres where excluded at start
  ## redefine fids to bring them back in
  if(exists("all_fids")){
     fids = all_fids
  }
  ## k cluster ids
  m = match(fids, names(k))
  k = k[m]
  ## list of independent features (0|1, 1 = yes)
  w = which(fids %in% independent_features)
  independent_features_binary = rep(0, length(fids))
  independent_features_binary[w] = 1
  
  out = data.frame( feature_ID = fids, k = k, 
                    independent_features_binary = independent_features_binary)
  
  dataout = list(speartree = stree, 
                 independent_features = independent_features, 
                 dataframe = out)
  
  return(dataout)
}



