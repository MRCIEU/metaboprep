#' A Function to describe, plot the distribution of and identifiy outliers for every metabolite (column) in a data frame.
#'
#' This function allows you to 
#' @param dtst metabolite data frame
#' @param pdf_filename name of the pdf out file 
#' @param nsd number of SD to consider as outliers, 5 is default 
#' @keywords metabolomics
#' @export
#' @examples
#' outlier.summary()
outlier.summary = function(dtst, pdf_filename = "./feature_distributions.pdf", nsd = 5){
  
  ## transform dataset
  # data_t <- as.data.frame(t(dtst))
  data_t = dtst
  
  ## set up results tables
  outlier_results = as.data.frame(matrix(data = 0, nrow = nrow(data_t), ncol = ncol(data_t)))
  
  # Center and Scale all data
  data_t <- apply(data_t, 2, function(x){  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)  })
  
  # save .pdf with summaries
  pdf(pdf_filename)
  par(mfrow=c(3,2))
  
  ## loop over features
  for (i in 1:ncol(data_t)){
    # continue only if more than one observation
    if (length(which(!is.na(data_t[,i]))) > 1) {
      mtb_name <- names(data_t[i])
      # id outliers
      threshold_SDplus <- mean(data_t[,i],na.rm=T) + (nsd*(sd(data_t[,i],na.rm=T)))
      threshold_SDminus <- mean(data_t[,i],na.rm=T) - (nsd*(sd(data_t[,i],na.rm=T)))
      upperoutliers <- which(data_t[,i] > threshold_SDplus)
      loweroutliers <- which(data_t[,i] < threshold_SDminus)
      outliers <- c(upperoutliers,loweroutliers)
      # Plot concentration/proportion of metabolite vs observation number 
      y_lim <- c( min(c(max(data_t[,i],na.rm=T),threshold_SDminus),na.rm=T), 
                  max(c(max(data_t[,i],na.rm=T),threshold_SDplus),na.rm=T) )
      plot(seq(1,nrow(data_t),1),(data_t[,i]),ylim=y_lim,
           main=(colnames(data_t)[i]),xlab="sample index",
           ylab="peak area",pch=21,bg="dodgerblue")
      abline(h=threshold_SDplus,col="red")   # +nsd
      text(nrow(data_t)/2,threshold_SDplus, paste0("plus_",nsd,"SD"), 
           col="red",cex=0.6, pos = 1,xpd = NA)
      if (threshold_SDminus > 0) {
        abline(h=threshold_SDminus,col="red")   # -nsd
        text(nrow(data_t)/2,threshold_SDminus, paste0("minus_",nsd,"SD"),
             col="red",cex=0.6, pos = 3,xpd = NA)
      }
      # Plot histogram with distribution statistics
      DataDescribed = psych::describe(data_t[,i])
      meanvar<-DataDescribed$mean
      medianvar<-DataDescribed$median
      minvar<-DataDescribed$min
      maxvar<-DataDescribed$max
      kurtosisvar<-DataDescribed$kurtosis
      skewnessvar<-DataDescribed$skew
      N<- nrow(data_t) - sum(is.na(data_t[,i]))
      missingness <- (sum(is.na(data_t[,i]))/nrow(data_t))*100
      
      a<-density(data_t[,i], na.rm=T)
      thresholdx<-(maxvar+(maxvar/100))
      thresholdy<-min(a$y)+(max(a$y)/4)
      
      hist(data_t[,i], col="red",main=(names(data_t)[i]),prob=TRUE,xlab="peak area") 
      lines(density(data_t[,i], na.rm = TRUE),col="blue", lwd=2)
      text(thresholdx,thresholdy, cex=0.6, 
           paste("N=", N, "\npercent missing=", 
                 signif(missingness, 3), "\nmin=", 
                 signif(minvar, 3), " \nmax=",
                 signif(maxvar, 3), 
                                                 "\nmean=", 
                 signif(meanvar, 3), " \nmedian=", signif(medianvar, 3), 
                                                 "\nkurt=", 
                 signif(kurtosisvar, 3), " \nskew=", 
                 signif(skewnessvar, 3), sep = ''), pos = 3,xpd = NA)
      # output flag info
      outlier_results[c(outliers),i] <- 1
  
    }
    else {
      outlier_results[,i] <- NA
    }
  }
  dev.off()
  
  ## Outlier table
  names(outlier_results) <- names(data_t)
  row.names(outlier_results) <- row.names(data_t)
  
  ## table figure
  fbys = quantile( apply(outlier_results, 1, function(x){  sum(x, na.rm = TRUE)}) )
  sbyf = quantile( apply(outlier_results, 2, function(x){  sum(x, na.rm = TRUE)}) )
  outtable = data.frame( percentile = c("0%","25%","50%","75%","100%"), 
                         outlying_features_by_sample = fbys,
                         outlying_samples_by_feature = sbyf)
  
  outtable <- ggpubr::ggtexttable(outtable, rows = NULL, 
                          theme = ggpubr::ttheme("mBlue"))
  
  
  return(list(outlier_results = outlier_results, outtable = outtable))
}

