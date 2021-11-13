#' greedy selection
#'
#' This function identifies features that have less than a minimum number of complete pairwise observations and removes one of them, in a greedy fashion.
#' The need for this function is in instances where missingness is extreme between two features the number of paired observation between them may be to
#' to be informative. Thus one, but not both should be removed from the analysis to avoid analytical error based on sample sizes.
#'
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param minN the minimum sample size (n) for pairwise comparisons
#'
#' @keywords feature selection
#' 
#' @return a vector of feature names
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' ex_data = sapply(1:10, function(x){ rnorm(250, 40, 5) })
#' ## define the data set
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## add in some missingness
#' ex_data[ sample(1:250, 200) ,1] = NA
#' ex_data[ sample(1:250, 200) ,2] = NA
#' ex_data[ sample(1:250, 200) ,3] = NA
#' ## Estimate missingness and generate plots
#' greedy.pairwise.n.filter(ex_data)
#' 
#'
greedy.pairwise.n.filter = function(wdata, minN = 50){
	## feature names
	fids = colnames(wdata)
  	cat(paste0("\t\t\t- Estimate Pairwise Sample Size (n) Matrix.\n"))
  	## make an empty matrix
  	Nmat = matrix(NA, ncol(wdata), ncol(wdata), dimnames = list(fids, fids))

  	## loop over features
  	for(i in 1:length(fids)){
  		for(j in 1:length(fids)){
  			w = which( !is.na(wdata[,i]) & !is.na(wdata[,j]) )
  			n = length(w)
  			Nmat[i,j] = n
  		}
  	}

  	cat(paste0("\t\t\t- Greedy removal of one feature in a pair of features with minimum n < ", minN ,".\n"))
  	## are there any comparisons smaller than minN
  	min_count = apply(Nmat, 2, function(x){ sum( x < minN ) })
  	
  	features2_remove = c()
  	iteration = 0
  	while( sum(min_count) > 0 ){
  		iteration = iteration + 1
  		cat(paste0("\t\t\t\t- Iteration ", iteration ," of while loop.\n"))
  		
  		## which feature has the most pairs
  		w = which(min_count == max(min_count))

  		## if there are more than one feature with equal counts
  		## remove the one with the most missingness
  		if(length(w) > 1){
  			n = names(w)
  			## estimate missingess
  			fmis = apply(wdata[, n], 2, function(x){ sum(is.na(x))/length(x) })
  			o = order(fmis, decreasing = TRUE)
  			w = w[o[1]]  ## feature with the most missingness
  		}
  		## retain the feature name
  		features2_remove = c(features2_remove, names(w))
  		## redefine the sample size matrix
  		Nmat = Nmat[-w, -w]
  		## re-estiamte the minimum count
  		min_count = apply(Nmat, 2, function(x){ sum( x < minN ) })
  	} ## end of while loop

  	return(features2_remove)
}




