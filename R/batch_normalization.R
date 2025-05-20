#' median batch normalization
#'
#' This function median normalizes multivariable data often processed in batches, such as metabolomic and proteomic data sets.
#'
#'
#' @param wdata the metabolite data frame samples in row, metabolites in columns
#' @param feature_data_sheet a data frame containing the feature annotation data
#' @param sample_data_sheet a data frame containing the sample annotation data
#' @param feature_runmode_col a string identifying the column name in the feature_data_sheet that identifies the run mode for each feature (metabolites of proteins).
#' @param batch_ids a string vector, with a length equal to the number of samples in the data set that identifies what batch each sample belongs to. 
#'
#' @keywords metabolomics proteomics batch normalization
#'
#' @return returns the wdata object passed to the function median normalized given the batch information provided.
#'
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#' ####################################
#' ## with a vector of batch variables
#' ####################################
#' ## define the data set
#' d1 = sapply(1:10, function(x){ rnorm(25, 40, 2) })
#' d2 = sapply(1:10, function(x){ rnorm(25, 35, 2) })
#' ex_data = rbind(d1,d2)
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## define the batch
#' batch = c( rep("A", 25), rep("B", 25)  )
#' ## normalize by batch
#' norm_wdata = batch_normalization(wdata = ex_data, batch_ids = batch )
#' 
batch_normalization = function( wdata, feature_data_sheet = NULL, sample_data_sheet = NULL, feature_runmode_col = NULL, batch_ids = NULL  ){
  ##
  if( length(batch_ids) == 0 ){
    ## extract platform assignment for each feature (meatbolite of protein)
    platforms = unlist( feature_data_sheet[, feature_runmode_col] )
    
    ## confirm number of features in the feature_data_sheet corresponds to the number of 
    ## features in the wdata data frame
    if( nrow(feature_data_sheet) != ncol(wdata) ){
      stop("There is a problem in running the function batch_normalization().
           The number of rows (features) in the feature_data_sheet does not equal 
           the number of columns (features) in the wdata (metabolite data) data frame.")
    }
    
    ## edit the string to ensure they match what is provided in the sample data sheet
    platforms = tolower(platforms)
    platforms = gsub("_","",platforms) # remove underscores
    platforms = gsub("-","",platforms) # remove dashes
    platforms = gsub("\\.","",platforms) # remove periods
    platforms = gsub(" ","",platforms) # remove spaces
    platformIDs = sort( unique(platforms) )
    
    ## edit the column names of the sample sheet
    colnames(sample_data_sheet) = tolower(colnames(sample_data_sheet))
    colnames(sample_data_sheet) = gsub("_","", colnames(sample_data_sheet)) # remove underscores
    colnames(sample_data_sheet) = gsub("-","",colnames(sample_data_sheet)) # remove dashes
    colnames(sample_data_sheet) = gsub("\\.","",colnames(sample_data_sheet)) # remove periods
    colnames(sample_data_sheet) = gsub(" ","",colnames(sample_data_sheet)) # remove spaces
    
    ## confirm that the platformIDs found in the feature data sheet
    ##  match some colnames in the sample data sheet
    if( sum( platformIDs %in% colnames(sample_data_sheet) ) != length(platformIDs) ){
      stop("There is a problem in running the function batch_normalization(). 
      Unable to match all run mode names in feature_data_sheet to column names in the sample_data_sheet.
      Please be sure that there is a column in the feature_data_sheet that identifes which run_mode each feature (metabolite) was extracted from.
      Then be sure there is a column name in the sample_data_sheet that corresponds to that run_mode name. 
      This later column should define the batch ids for each sample for that run mode.")
    }
    
    ## iterate over the platforms, their metabolites, and those platform-x-metabolite batches
    for(plat in platformIDs){
      
      ## identify the metabolites that belong to a platform
      metabolite_index = which( platforms == plat) 
      
      ## identify the platform batch IDs
      batches =  as.character( unlist( sample_data_sheet[, plat] ) ) 
      batchIDs = unique( batches )
      
      ## perform the normalization
      for(bid in batchIDs){
        sample_index = which(batches == bid)
        m = apply( wdata[sample_index, metabolite_index], 2, function(x){ stats::median(x, na.rm = TRUE) })
        for(j in 1:length(m)){
          wdata[ sample_index, metabolite_index[j] ] = wdata[ sample_index, metabolite_index[j] ] / m[j]
        }
        
      }
    } ## END of for(plat in platformIDs) statement
    
  } else { ## run normalization with a single vector of batch iDs provided by the variable 'batch_ids'
      
      ## confirm number of elements in the batch_ids vector corresponds to the number of 
      ## samples in the wdata data frame
      if( length(batch_ids) != nrow(wdata) ){
        stop("There is a problem in running the function batch_normalization().
             The number of strings in the supplied 'batch_ids' vector does not equal 
             the number of rows (samples) in the wdata (metabolite data) data frame.")
      }
      
      ## identify the platform batch IDs
      batches =  as.character( unlist( batch_ids ) ) 
      batchIDs = unique( batches )
      
      ## perform the normalization
      for(bid in batchIDs){
        sample_index = which(batches == bid)
        m = apply( wdata[sample_index, ], 2, function(x){ median(x, na.rm = TRUE) })
        for(j in 1:length(m)){
          wdata[ sample_index, j ] = wdata[ sample_index, j ] / m[j]
        }
      }
  }
  
  return(wdata)
}
