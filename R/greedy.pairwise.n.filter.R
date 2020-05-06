#' A Function to identify features that have less than a minimum number of complete pairwise observations and remove one of them, in a greedy fashion.
#'
#' This function allows you to generate a dendrogram of feautres based on correlation coefficeint of your choice, and a clustering method of choice
#' @param wdata the metabolite data matrix. samples in row, metabolites in columns
#' @param minN the minimum sample size (n) for pairwise comparisons
#' @keywords feature selection
#' @export
#' @examples
#' greedy.pairwise.n.filter()
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

  	cat(paste0("\t\t\t- Greedy removal of one feature in a pair of feautres with minimum n < ", minN ,".\n"))
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




