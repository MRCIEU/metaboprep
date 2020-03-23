#' A Super Function to perform all of your metabolomics QC
#'
#' This function allows you perform the key quality controls steps on a metabolomics data set
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param fmis defaulted at 0.2, this defines the feature missingness cutoff
#' @param smis defaulted at 0.2, this defines the sample missingness cutoff
#' @param feature_colnames_2_exclude names of columns to be excluded from this analysis, such as for Xenobiotics. Pass NA to this paramater to exclude this step.
#' @param tpa_out_SD defaulted at '5', this defines the number of standard deviation from the mean in which samples will be excluded for total peak area. Pass NA to this paramater to exclude this step.
#' @param PC_out_SD defaulted at '5', this defines the number of standard deviation from the mean in which samples will be excluded for principle components. NA is NOT an excepted paramater.
#' @param ind_feature_names If you have previously identified independent features in your data set they can be provided here a vector of column names. Pass NA to this paramater to have the function estimate indpendence for you.
#' @keywords metabolomics
#' @export
#' @examples
#' perform.metabolite.qc()
perform.metabolite.qc = function(wdata, fmis = 0.2, smis = 0.2, 
                                 feature_colnames_2_exclude = NA, 
                                 tpa_out_SD = 5, 
                                 PC_out_SD = 5,
                                 ind_feature_names = NA ){
  ## remove and retain exclusions
  if( is.na( feature_colnames_2_exclude[1] ) == FALSE ){
    if( length(feature_colnames_2_exclude) > 0){
      cat( paste0("\t\t- QCstep: extracting exclusion features from analysis.\n") )
      w = which( colnames(wdata)  %in% feature_colnames_2_exclude )
      exdata = wdata[ , w ]
      wdata = wdata[ , -w ]
      
    }
  }
  
  ## 1) estimate inital sample missingness
  cat( paste0("\t\t- QCstep: estimate initial sample missingness.\n") )
  samplemis = sample.missingness(wdata)
  
  ## 2) exclude terrible samples (smis > 0.8)
  cat( paste0("\t\t- QCstep: exclude those sample with missingness > 80%.\n") )
  r = which(samplemis[,1] >= 0.8)
  if( length(r) > 0) { 
    cat( paste0("\t\t\t* ", length(r), " samples excluded for extreme missingness.\n") )
    wdata = wdata[ -r, ]; samplemis = samplemis[-r] 
  } else {
    cat( paste0("\t\t\t* 0 samples excluded for extreme missingness.\n") )
  }
  
  ## 3) estimate inital feature missingness
  cat( paste0("\t\t- QCstep: estimate initial feature missingness.\n") )
  featuremis = feature.missingness(wdata = wdata, samplemissingness = samplemis)
  
  ## 4) exclude terrible features (fmis > 0.8)
  cat( paste0("\t\t- QCstep: exclude those features with missingness > 80%.\n") )
  r = which(featuremis >= 0.8)
  if( length(r) > 0) {  
    cat( paste0("\t\t\t* ", length(r), " features excluded for extreme missingness.\n") )
    wdata = wdata[ , -r ]  
  } else {
    cat( paste0("\t\t\t* 0 features excluded for extreme missingness.\n") )
  }
  
  ## 5) re-estimate  sample missingness
  cat( paste0("\t\t- QCstep: RE-estimate sample missingness.\n") )
  samplemis = sample.missingness(wdata)
  
  ## 6) exclude samples defined by user ( smis > 0.2 (default) )
  cat( paste0("\t\t- QCstep: exclude those sample with missingness > ", smis*100,"%.\n") )
  r = which(samplemis[,1] >= smis )
  if( length(r) > 0) { 
    cat( paste0("\t\t\t* ", length(r), " samples excluded for user defined missingness.\n") )
    wdata = wdata[ -r, ]
    samplemis = samplemis[-r] 
  } else {
    cat( paste0("\t\t\t* 0 samples excluded for user defined missingness.\n") )
  }
  
  ## 7) estimate inital feature missingness
  cat( paste0("\t\t- QCstep: RE-estimate feature missingness.\n") )
  featuremis = feature.missingness(wdata = wdata, samplemissingness = samplemis)
  
  ## 8) exclude terrible features ( fmis > 0.2 (default) )
  cat( paste0("\t\t- QCstep: exclude those features with missingness > ", fmis*100,"%.\n") )
  r = which(featuremis >= fmis)
  if( length(r) > 0) {  
    cat( paste0("\t\t\t* ", length(r), " features excluded for user defined missingness.\n") )
    wdata = wdata[ , -r ]  
  } else {
    cat( paste0("\t\t\t* 0 features excluded for user defined missingness.\n") )
  }
  
  ### 9) total peak area
  cat( paste0("\t\t- QCstep: estimate total peak area.\n") )
  tpa = total.peak.area(wdata)
  ## if you want to filter on TPA
  if( is.na(tpa_out_SD) == FALSE){
    cat( paste0("\t\t- QCstep: exclude those features with TPA > ", tpa_out_SD,"SD from the mean.\n") )
    s = sd( tpa[,1] ); m = mean( tpa[,1] )
    sdout = m + (tpa_out_SD * s)
    w = which(tpa[,1] >= sdout | tpa[,1] <= -sdout )
    if( length(w) > 0 ){
      cat( paste0("\t\t\t* ", length(w), " samples excluded for user defined TPA SD from the mean.\n") )
      wdata = wdata[ -w, ]
    } else {
    cat( paste0("\t\t\t* 0 samples excluded for user defined TPA SD from the mean.\n") )
    }
  }
  
  ### 10) feature independence and PC outliers
  if( is.na(ind_feature_names[1]) == TRUE ){
    cat( paste0("\t\t- QCstep: identify independent features through correlation analysis and dendrogram clustering.\n") )
    ## we need to estimate independent features denovo, if not available
    featuresumstats = feature.sum.stats( wdata = wdata, sammis = samplemis)
    w = which(featuresumstats$table$independent_features_binary == 1)
    ind_feature_names = rownames(featuresumstats$table)[w]
    cat( paste0("\t\t\t* ", length(ind_feature_names), " independent features identified.\n") )
  }
  
  ## Take the list of independent features and find them
  ## in the wdata data frame.
    # k = which(colnames(wdata) %in% ind_feature_names)
  
  ## binary index of binary features needed for the pc.and.outliers
  ## function
  cat( paste0("\t\t- QCstep: Perform Principle Componenet Analysis of currently QC'd data.\n") )
  PCs_outliers = pc.and.outliers(metabolitedata =  wdata, 
                                 indfeature_names = ind_feature_names )
  pcs = PCs_outliers[[1]][, 1:2]

  ## identify PC outliers
  cat( paste0("\t\t- QCstep: Identify PC 1-&-2 outliers at > ", PC_out_SD , "SD of the mean.\n") )
  outliers = outlier.matrix(pcs, nsd = PC_out_SD)
  outliers = apply(outliers, 1, sum)
  w = which(outliers>0)
  if(length(w)>0){
    cat( paste0("\t\t\t* ", length(w), " samples excluded as PC outliers.\n") )
    wdata = wdata[-w, ]
  } else {
    cat( paste0("\t\t\t* 0 samples excluded as PC outliers.\n") )
    }
  
  ## 11) put the exclusion features back
  if( exists("exdata") ){
    cat( paste0("\t\t- QCstep: placing the initially extracted exclusion features back into the data frame.\n") )
    ## match sample ids
    m = match(rownames(wdata), rownames(exdata))
    wdata = cbind(wdata, exdata[m, ])
  }
  
  ##
  return(wdata)
}




