#' A Super Function to perform all of your metabolomics QC
#'
#' This function allows you perform the key quality controls steps on a metabolomics data set
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param fmis defaulted at 0.2, this defines the feature missingness cutoff
#' @param smis defaulted at 0.2, this defines the sample missingness cutoff
#' @param feature_colnames_2_exclude names of columns to be excluded from all analysis, such as for Xenobiotics. Pass NA to this paramater to exclude this step.
#' @param derived_colnames_2_exclude names of columns to exclude from all sample QC steps, including independnet feature identification, which is used to generate the sample PCA.
#' @param tpa_out_SD defaulted at '5', this defines the number of standard deviation from the mean in which samples will be excluded for total peak area. Pass NA to this paramater to exclude this step.
#' @param PC_out_SD defaulted at '5', this defines the number of standard deviation from the mean in which samples will be excluded for principle components. NA is NOT an excepted paramater.
#' @param tree_cut_height The height at which to cut the feature|metabolite dendrogram to identify "independent" features. tree_cut_height is 1-absolute(Spearman's Rho) for intra-cluster correlations.
#' @keywords metabolomics
#' @export
#' @examples
#' perform.metabolite.qc()
perform.metabolite.qc = function(wdata, fmis = 0.2, smis = 0.2, 
                                 feature_colnames_2_exclude = NA,
                                 derived_colnames_2_exclude = NA, 
                                 tpa_out_SD = 5, 
                                 PC_out_SD = 5,
                                 tree_cut_height = 0.5 ){
  ## remove and retain exclusions
  if( is.na( feature_colnames_2_exclude[1] ) == FALSE ){
    if( length(feature_colnames_2_exclude) > 0){
      cat( paste0("\t\t- QCstep: extracting exclusion features from analysis.\n") )
      w = which( colnames(wdata)  %in% feature_colnames_2_exclude )
      exdata = wdata[ , w ]
      wdata = wdata[ , -w ]
      
    }
  }
  
  ## Record of exclusions made
  exclusion_data = matrix(NA, nrow = 6, ncol = 1, dimnames = list( c("Extreme_sample_missingness","Extreme_feature_missingness","User_defined_sample_missingness","User_defined_feature_missingness","User_defined_sample_totalpeakarea","User_defined_sample_PCA_outliers"),c("count") ) )

  ## 1) estimate inital sample missingness
  cat( paste0("\t\t- QCstep: estimate initial sample missingness.\n") )
  if( !is.na(derived_colnames_2_exclude[1]) ){
    samplemis = sample.missingness(wdata, excludethesefeatures = derived_colnames_2_exclude)
    } else {
      samplemis = sample.missingness(wdata, excludethesefeatures = NA)
    }
  
  
  ## 2) exclude terrible samples (smis > 0.8)
  cat( paste0("\t\t- QCstep: exclude those sample with missingness >= 80%.\n") )
  if( ncol(samplemis) == 2){
    r = which(samplemis[,2] >= 0.8)
    } else {
      samplemis$sample_missingness_w_exclusions = samplemis$sample_missingness
      r = which(samplemis[,1] >= 0.8)    
    }
  exclusion_data[1,1] = length(r)
  ###
  if( length(r) > 0) { 
    cat( paste0("\t\t\t* ", length(r), " sample(s) excluded for extreme missingness.\n") )
    wdata = wdata[ -r, ]; samplemis = samplemis[-r,] 
  } else {
    cat( paste0("\t\t\t* 0 samples excluded for extreme missingness.\n") )
  }
  
  ## 3) estimate inital feature missingness
  cat( paste0("\t\t- QCstep: estimate initial feature missingness.\n") )
  if( ncol(samplemis) == 2){
    SM = samplemis[,2]
    } else {
      SM = samplemis[,1]
    }
  featuremis = feature.missingness(wdata = wdata, samplemissingness = NA)
  
  ## 4) exclude terrible features (fmis > 0.8)
  cat( paste0("\t\t- QCstep: exclude those features with missingness >= 80%.\n") )
  r = which(featuremis >= 0.8)
  exclusion_data[2,1] = length(r)
  ##
  if( length(r) > 0) {  
    cat( paste0("\t\t\t* ", length(r), " feature(s) excluded for extreme missingness.\n") )
    wdata = wdata[ , -r ]  
  } else {
    cat( paste0("\t\t\t* 0 features excluded for extreme missingness.\n") )
  }
  
  ## 5) re-estimate sample missingness
  cat( paste0("\t\t- QCstep: RE-estimate sample missingness.\n") )
  if( !is.na(derived_colnames_2_exclude[1]) ){
    samplemis = sample.missingness(wdata, excludethesefeatures = derived_colnames_2_exclude)
    } else {
      samplemis = sample.missingness(wdata, excludethesefeatures = NA)
    }
  
  ## 6) exclude samples defined by user ( smis >= 0.2 (default) )
  cat( paste0("\t\t- QCstep: exclude those sample with missingness >= ", smis*100,"%.\n") )
  if( ncol(samplemis) == 2){
    r = which(samplemis[,2] >= smis)
    } else {
      samplemis$sample_missingness_w_exclusions = samplemis$sample_missingness
      r = which(samplemis[,1] >= smis)    
    }
  exclusion_data[3,1] = length(r)
  ##
  if( length(r) > 0) { 
    cat( paste0("\t\t\t* ", length(r), " sample(s) excluded for user defined missingness.\n") )
    wdata = wdata[ -r, ]
    samplemis = samplemis[-r,] 
  } else {
    cat( paste0("\t\t\t* 0 samples excluded for user defined missingness.\n") )
  }
  
  ## 7) re-estimate feature missingness
  cat( paste0("\t\t- QCstep: RE-estimate feature missingness.\n") )
  if( ncol(samplemis) == 2){
    SM = samplemis[,2]
    } else {
      SM = samplemis[,1]
    }
  featuremis = feature.missingness(wdata = wdata, samplemissingness = NA)
  
  ## exclude features based on user defined feature missingness ( fmis > 0.2 (default) )
  cat( paste0("\t\t- QCstep: exclude those features with missingness >= ", fmis*100,"%.\n") )
  r = which(featuremis[,1] >= fmis)
  exclusion_data[4,1] = length(r)
  ##
  if( length(r) > 0) {  
    cat( paste0("\t\t\t* ", length(r), " feature(s) excluded for user defined missingness.\n") )
    wdata = wdata[ , -r ]  
  } else {
    cat( paste0("\t\t\t* 0 features excluded for user defined missingness.\n") )
  }
  
  ### 9) total peak area
  cat( paste0("\t\t- QCstep: estimate total peak area.\n") )
  if( !is.na(derived_colnames_2_exclude[1]) ){
    tpa = total.peak.area(wdata, feature_names_2_exclude = derived_colnames_2_exclude)
    } else {
      tpa = total.peak.area(wdata, feature_names_2_exclude = NA)    
    }
  
  
  ## if you want to filter on TPA
  if( is.na(tpa_out_SD) == FALSE){
    cat( paste0("\t\t- QCstep: exclude those features with TPA >= ", tpa_out_SD,"SD from the mean.\n") )
    s = sd( tpa[,1] ); m = mean( tpa[,1] )
    sdout = m + (tpa_out_SD * s)
    w = which(tpa[,1] >= sdout | tpa[,1] <= -sdout )

    exclusion_data[5,1] = length(w)

    if( length(w) > 0 ){
      cat( paste0("\t\t\t* ", length(w), " sample(s) excluded for user defined TPA SD from the mean.\n") )
      wdata = wdata[ -w, ]
    } else {
    cat( paste0("\t\t\t* 0 samples excluded for user defined TPA SD from the mean.\n") )
    }
  } else {
    cat( paste0("\t\t\tYou have chosen NOT to apply a QC-filter on individuals total peak area.\n") )
    cat( paste0("\t\t\ttotal_peak_area_SD in the parameter file was set to NA.\n") )
  }
  
  ### 10) re-identify feature independence and PC outliers
  cat( paste0("\t\t- QCstep: re-identify independent features through correlation analysis and dendrogram clustering.\n") )
  cat( paste0("\t\t\t- using currently QCd data.\n") )
  
  ## re-estimate independent features using the qc-data to this point
  if( !is.na(derived_colnames_2_exclude[1]) ){
    featuresumstats = feature.sum.stats( wdata = wdata, sammis = samplemis, tree_cut_height = tree_cut_height, feature_names_2_exclude = derived_colnames_2_exclude)
    } else {
      featuresumstats = feature.sum.stats( wdata = wdata, sammis = samplemis, tree_cut_height = tree_cut_height, feature_names_2_exclude = NA)
    }
  
  ## extract independent feature list
  w = which(featuresumstats$table$independent_features_binary == 1)
  ind_feature_names = rownames(featuresumstats$table)[w]
  cat( paste0("\t\t\t* ", length(ind_feature_names), " independent features identified.\n") )
  
  
  ## identify PC outliers
  cat( paste0("\t\t- QCstep: Perform Principle Componenet Analysis of currently QC'd data.\n") )
  PCs_outliers = pc.and.outliers(metabolitedata =  wdata, 
                                 indfeature_names = ind_feature_names )
  ## extract PCs 1-2 or 1-number of Acceleration factor PCs 
  af = as.numeric( PCs_outliers[[3]] )
  if(af<2){
    pcs = PCs_outliers[[1]][, 1:2]  
  } else {
    pcs = PCs_outliers[[1]][, 1:af]  
  }
  
  ## perform exclusion on top PCs to ID outliers
  cat( paste0("\t\t- QCstep: Identify PC 1-",af," outliers >= +/-", PC_out_SD , "SD of the mean.\n") )
  if( is.na(PC_out_SD) == FALSE){
    outliers = outlier.matrix(pcs, nsd = PC_out_SD)
    outliers = apply(outliers, 1, sum)
    w = which(outliers>0)

    exclusion_data[6,1] = length(w)

    if(length(w)>0){
      cat( paste0("\t\t\t* ", length(w), " samples excluded as PC outliers.\n") )
      wdata = wdata[-w, ]
    } else {
      cat( paste0("\t\t\t* 0 samples excluded as PC outliers.\n") )
      }
  } else {
    cat( paste0("\t\t\tYou have chosen NOT to apply a QC-filter on individuals based on their PC eigenvectors.\n") )
    cat( paste0("\t\t\tPC_outlier_SD in the parameter file was set to NA.\n") )
  }
  
  ## 11) put the exclusion features back
  if( exists("exdata") ){
    cat( paste0("\t\t- QCstep: placing the initially extracted exclusion features back into the data frame.\n") )
    ## match sample ids
    m = match(rownames(wdata), rownames(exdata))
    wdata = cbind(wdata, exdata[m, ])
  }
  
  ##
  return( list( wdata = wdata, featuresumstats = featuresumstats, pca = PCs_outliers, exclusion_data = exclusion_data) )
}




