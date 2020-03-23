#' A Function to identify independent features
#'
#' This function allows you to identify independent features using Spearman's Rho, and a dendrogram tree cut step.
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @keywords metabolomics
#' @export
#' @examples
#' feature.tree.independence()
feature.tree.independence = function(wdata){
  ## vector of feature ids
  fids = colnames(wdata)
  ## identify features with no variance
  rowvar0 <- which( apply(wdata,2,function(x) var(x,na.rm=T)==0) )
  if(length(rowvar0) > 0){ wdata = wdata[, -rowvar0] }
  ## identify features with complete data
  fmis = apply(wdata, 2, function(x){ sum(is.na(x))/length(x) }) 
  f_remove = which(fmis > 0.2 )
  if(length(f_remove) > 0){  wdata = wdata[, -f_remove] }
  ## identify feature names that are percentages "pct"
  w = which( sapply(colnames(wdata), function(x){
    o = substr(x, nchar(x)-2,nchar(x) ) == "pct"
    }) )
  if(length(w) > 0){
    wdata = wdata[, -w]
    }
  
  ###########
  # make tree
  ###########
  stree <- make.tree(wdata=wdata, 
                     cor_method="spearman",
                     hclust_method="complete")
  
  # restrict based on cut off
  k = cutree(stree, h = 0.20)
  
  # restrict so as to keep the first feature in each cluster
  # independent_features <- names(k[unique(k)])
  
  # restrict so as to keep the feature with the least missingness
  k_group = table(k)
  ## strictly independent features
  w = which(k_group == 1); ind_k = names( k_group[w] ); w = which(k %in% ind_k)
  ind = names(k)[w]
  ## indentify feature with least missingness in each group
  w = which(k_group > 1); k_group =  names( k_group[w] )
  ind2 = sapply(k_group, function(x){
    w = which(k %in% x); n = names( k[w] )
    o = sort(fmis[n], decreasing = FALSE)
    out = names(o)[1]
    return(out)
  })
  ind_feat = paste( c(ind, ind2) )
  
  ## set up returning vectors (out data)
  ## k cluster ids
  m = match(fids, names(k))
  k = k[m]
  ## list of independent features (0|1, 1 = yes)
  w = which(fids %in% ind_feat)
  independent_features = rep(0, length(fids))
  independent_features[w] = 1
  
  ### data to return
  dataout = data.frame( k = k, 
                  independent_features = independent_features)
  
  return(dataout)
}




