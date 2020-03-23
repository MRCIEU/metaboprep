#' A Function to plot PCs in a matrix
#'
#' This function to plot PCs in a matrix
#' @param myloadings PC loadings
#' @param varexp the variance explained by each PC in a vector.
#' @keywords metabolomics
#' @export
#' @examples
#' pcapairs_bymoose()
pcapairs_bymoose = function(myloadings, varexp){
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
  layout(m)
  par(mar = c(5,5,1,1))
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
        plot(myloadings[,j], myloadings[,i], pch = 21, bg = "dodgerblue",
             ylab = paste0("PC",i, " VarExp = ", signif(varexp[i], d = 3)*100, "%"),
             xlab = paste0("PC",j, " VarExp = ", signif(varexp[j], d = 3)*100, "%"))
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
