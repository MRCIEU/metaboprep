#' feature plots to file
#'
#' This function to plots a scatter plot, a histogram, and a few summary statistics of each feature in a data frame to a pdf file
#'
#' @param wdata a data frame of feature (ex: metabolite or protein) abundance levels
#' @param outdir output directory path
#' @param nsd number of SD from the mean to plot an outlier line on the scatter plot and histogram
#'
#' @keywords metabolomics summary pdf
#'
#' @importFrom ggpubr ggarrange ggtexttable ggexport
#' @importFrom RColorBrewer brewer.pal
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_point geom_histogram geom_hline geom_vline theme_bw
#' 
#' @return a ggplot2 object
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:20, function(x){ rnorm(250, 40, 5) })
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' feature_plots(ex_data)
#'
feature_plots <- function(wdata, outdir=NULL, nsd = 5){
  ## define local function variable
  index <- level <- NULL
  ## package check
  pkgs = c("ggpubr", "RColorBrewer", "magrittr", "ggplot2")
  for(pkg in pkgs){
    if (!requireNamespace( pkg, quietly = TRUE)) {
        stop(paste0("Package \"", pkg,"\" needed for feature_plots() function to work. Please install it."),call. = FALSE)
      }
  }

  ## extract trait names from columns
  traits = sort(colnames(wdata) )
  
  ## plot parameter
  pcol = RColorBrewer::brewer.pal(8, "Set1")
  
  ## Make a list object with the plots in it
  plotsout = lapply(traits, function(trait){
    ########################
    ## Data Frame for trait
    ########################
    df = data.frame(level = wdata[, trait], index = 1:nrow(wdata))
    
    ########################
    ## Summary Statistics
    ########################
    ss = metaboprep::feature.describe(df)[1,] 
    rownames(ss) = trait
    ## reorder sum stats
    ss = ss[c(1,11, 2:10, 12:16)]
    
    ## define the 5SD extreme line
    extreme_line = ss$mean + (nsd * ss$sd)
    
    ## scatter plot
    p1 = df %>% ggplot(aes(x = index, y = level)) +
      geom_point(fill = pcol[2], shape = 21, size = 3, alpha = 0.6) +
      labs(x = "sample index", y = "feature level", title = trait ) +
      geom_hline(yintercept = extreme_line, color = pcol[1], size = 1.5) +
      theme_bw()
    
    ## histogram
    p2 = df %>% ggplot(aes(x = level)) +
      geom_histogram( fill = pcol[3], alpha = 0.9, color = "white", bins=30) +
      labs(x = "frequency", x = "feature level", title = trait ) +
      geom_vline(xintercept = extreme_line, color = pcol[1], size = 1.5) +
      theme_bw()
    
    ## summary statistics table
    ss[1,-1] = formatC( unlist(ss[1,-1]), digits = 3)
    p3 = ggpubr::ggtexttable(ss)
    
    ## combine plots
    top = ggpubr::ggarrange(p1,p2, nrow = 1) 
    out = ggpubr::ggarrange(  top, p3, nrow = 2, heights = c(4,1) )
    
    ## return combined plot to lapply function
    out
  })
  
  ## make sure the outdir ends with a /
  if(length(outdir)>0){
    if(substring(outdir, nchar(outdir)) != "/"){
      outdir = paste0(outdir,"/")
    }
  }
  
  ## Print plots to file
  f = paste0(outdir, "feature_distributions.pdf")
  ggpubr::ggarrange(plotlist = plotsout, ncol = 1, nrow = 3) %>% 
    ggpubr::ggexport(filename = f, width = 13, height = 15)
  
}

