#' missingness summary plots
#'
#' This function sumarizes sample and feature missingness in tables and in a summary plot.
#'
#' @param mydata metabolite data matrix, with samples in rows and metabolite features in columns. 
#'
#' @keywords metabolomics missingness summary plots
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom ggpubr ggtexttable ttheme
#' @importFrom ggplot2 aes geom_histogram geom_vline labs theme_bw theme element_text
#' @importFrom stats quantile
#' 
#' @return a list object of length four with (1) a vector of sample missingess,(2) a vector of feature missingness ,(3) a table summarizing missingness ,(4) a list of ggplot2 plots for sample and feature histogram distribution and summary tables
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:100, function(x){ rnorm(250, 40, 5) })
#' ## define the data set
#' rownames(ex_data) = paste0("ind", 1:nrow(ex_data))
#' colnames(ex_data) = paste0("var", 1:ncol(ex_data))
#' ## add in some missingness
#' ex_data[sample(1:length(ex_data), 500)] = NA
#' ## Estimate missingness and generate plots
#' ms = missingness.sum(ex_data)
#' ## plots
#' ggpubr::ggarrange(plotlist = ms$plotsout, ncol = 2, nrow = 2)
#' 
#'
missingness.sum = function(mydata){
  ## define local variable
  d <- NULL
  ## package check
  pkgs = c("RColorBrewer", "magrittr", "tibble", "ggpubr", "ggplot2")
  for(pkg in pkgs){
    if (!requireNamespace( pkg, quietly = TRUE)) {
        stop(paste0("Package \"", pkg,"\" needed for missingness.sum() function to work. Please install it."),call. = FALSE)
      }
  }

  ## set color scheme
  pcol = RColorBrewer::brewer.pal(9, "Set1")

  # calculate sample missingness
  sample_missing = apply(mydata, 1, function(x){ sum(is.na(x))/length(x)  })
  
  # calculate feature missingness
  feature_missing <- apply(mydata, 2, function(x){ sum(is.na(x))/length(x)  })
  
  # quantile distribution of missingness
  missingness_table = tibble::as_tibble( data.frame( percentile = seq(0, 1, 0.25), 
                               samples = round( stats::quantile(sample_missing) , digits = 4),
                               features = round(  stats::quantile(feature_missing), digits = 4 ) ) )
  # a summary table figure
  sumtable <- ggpubr::ggtexttable(missingness_table, rows = NULL, 
                          theme = ggpubr::ttheme("mBlue"))
  
  ## a table to print
  #table2print = missingness_table %>% kable() %>%
  #  kable_styling(bootstrap_options = "striped", 
  #                full_width = F, position = "center",
  #                font_size = 15) %>%
  #  row_spec(3, bold = T, color = "white", background = "dodgerblue")
  
  ## generate plot
  s = tibble::tibble(d = sample_missing)
  f = tibble::tibble(d = feature_missing)
  
  plotA = s %>% ggplot(aes(d)) + 
    geom_histogram( fill = pcol[2] , bins = 25) + 
    #geom_density( size = 2, lwd = 1 ) +
    geom_vline( xintercept = median(s$d), color = pcol[1], size = 1) +
    labs(title = "sample missingness", x="", y="") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  plotB = f %>% ggplot(aes(d)) + 
    geom_histogram( fill = pcol[3], bins = 25 ) + 
    geom_vline(xintercept = median(f$d), color = pcol[1], size = 2) +
    labs(title = "feature missingness", x="", y="") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  #############################
  ##  Estimates of sample size 
  ##  given various levels of 
  ##  missingness
  #############################
  s = seq(0.1, 0.5, 0.1)
  fmiss_samplesize = sapply(s, function(x){
    nrow(mydata)*(1-x)
  })
  
  fmiss_samplesize = data.frame(Perc_Missing =  paste0(s*100, "% feature missingness"), 
                                Sample_Size = fmiss_samplesize)
  
  miss_samplesize_table <- ggpubr::ggtexttable(fmiss_samplesize, rows = NULL, 
                                       theme = ggpubr::ttheme("mBlue"))
  
  #################################
  ##  Estiamtes of sample size 
  ##  given various levels of 
  ##  missingness
  #############################
  s = seq(0.1, 0.5, 0.1)
  fmiss_samplesize = sapply(s, function(x){
    nrow(mydata)*(1-x)
  })
  
  fmiss_samplesize = data.frame(percent.missing =  paste0(s*100, "% feature missingness"), 
                                sample.size = fmiss_samplesize)
  
  miss_samplesize_table <- ggpubr::ggtexttable(fmiss_samplesize, rows = NULL, 
                                       theme = ggpubr::ttheme("mBlue"))
  
  #######################
  ## plots to return
  ## to user
  #######################
  plotsout = list(plotA= plotA, plotB = plotB, sumtable = sumtable, miss_samplesize_table = miss_samplesize_table)
  
  ## return data, tables, and figure to user
  out = list(sample_missing = sample_missing, feature_missing = feature_missing, 
             missingness_table = missingness_table, plotsout = plotsout)
  return(out)
  }