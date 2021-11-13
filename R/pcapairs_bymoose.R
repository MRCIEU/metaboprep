#' pca pairs plot
#'
#' This function generates an upper triangle PCA plot for all pairs of PC loadings provided.
#'
#' @param myloadings a matrix or data frame of PC loadings (only those you would like to plot).
#' @param varexp a vector of the the variance explained by each PC
#' @param pcol plot colors for the dots background
#'
#' @keywords metabolomics PCA pairs plot
#' 
#' @return a base R plot
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:5, function(x){rnorm( 50, 0, 2) })
#' ## add in some extreme values to a sample
#' ex_data[1,] = ex_data[1,] + sample( c(8, -8), ncol(ex_data), replace = TRUE )
#' ex_data[2,] = ex_data[2,] + sample( c(3, -3), ncol(ex_data), replace = TRUE )
#' ## plot
#' pcapairs_bymoose(myloadings = ex_data, 
#'     varexp = rep(1/ncol(ex_data), ncol(ex_data)),
#'     pcol = "tomato" )
#'
pcapairs_bymoose = function(myloadings, varexp, pcol = "dodgerblue"){
  ## The number of PCs
  npcs = ncol(myloadings)
  ### make a matrix for the plot layout
  m = matrix( 1:(npcs*npcs), nrow = npcs, ncol = npcs, byrow = TRUE)
  m[lower.tri(m)] = 0
  diag(m) = 0
  o = order( m[m != 0] )
  m[m != 0][o] = 1:sum(upper.tri(m))
  extra = sum(upper.tri(m))+1
  m[m == 0] = extra
  ##
  graphics::layout(m)
  graphics::par(mar = c(5,5,1,1))
  for(i in 1:npcs){
    for(j in 1:npcs){
      if(m[i,j] != extra ){
        ## sd outlier
        outliersV = unlist( outliers(myloadings[,j],3)[2:3] )
        outliersH = unlist( outliers(myloadings[,i],3)[2:3] )
        outliersV4 = unlist( outliers(myloadings[,j],4)[2:3] )
        outliersH4 = unlist( outliers(myloadings[,i],4)[2:3] )
        outliersV5 = unlist( outliers(myloadings[,j],5)[2:3] )
        outliersH5 = unlist( outliers(myloadings[,i],5)[2:3] )
        
        ## perform plot
        plot(myloadings[,j], myloadings[,i], pch = 21, bg = pcol,
             ylab = paste0("PC",i, " VarExp = ", signif(varexp[i], digits = 3)*100, "%"),
             xlab = paste0("PC",j, " VarExp = ", signif(varexp[j], digits = 3)*100, "%"))
        abline(v = outliersV, lwd = 1.5, col = "grey")
        abline(h = outliersH, lwd = 1.5, col = "grey")
        ## 4SD
        abline(v = outliersV4, lwd = 1.5, col = "orange")
        abline(h = outliersH4, lwd = 1.5, col = "orange")
        ## 5SD
        abline(v = outliersV5, lwd = 1.5, col = "red")
        abline(h = outliersH5, lwd = 1.5, col = "red")
        
        ## end of plot
        } 
      }
    }
  }
