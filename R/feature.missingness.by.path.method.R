#' A Function to evaluate metabolon metabolite feature missingness by super pathway and MS-methodlolgy
#'
#' This function to evaluate metabolon metabolite feature missingness by super pathway and MS-methodlolgy
#' @param fdata A tibble of feature data
#' @keywords metabolomics, metabolon
#' @export
#' @examples
#' feature.missingness.by.path.method()
feature.missingness.by.path.method = function(fdata){
  #############################
  ## Estimation and ordering
  ## by mean missingness
  #############################
  # Estimating the mean and confidence intervals for
  # missingness across super pathways
  super = fdata %>% group_by(SUPER_PATHWAY) %>%
    summarise( m =  mean(feature_missing),
               CI_L = quantile(feature_missing, 0.025),
               CI_H = quantile(feature_missing, 0.975)) %>%
    arrange(m)
  
  # Estimating the mean and confidence intervals for
  # missingness across platforms or methods
  platform = fdata %>%
    group_by(PLATFORM) %>%
    summarise( m = mean(feature_missing) ,
               CI_L = quantile(feature_missing, 0.025),
               CI_H = quantile(feature_missing, 0.975) ) %>%
    arrange(m)

  #############################
  ## SUPER PATHWAY PLOT
  #############################
  ##### order super pathway by mean missingness
  fdata$SUPER_PATHWAY = factor(fdata$SUPER_PATHWAY, levels = super$SUPER_PATHWAY, ordered = TRUE)
  ##
  plotA = fdata %>% group_by(SUPER_PATHWAY) %>% ggplot( aes(x = SUPER_PATHWAY, y = feature_missing)) +
    geom_boxplot( aes(color = SUPER_PATHWAY), notch = FALSE) +
    theme( axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(title = "Feature missingness by\nsuper pathway classification",
         y = "missingness", x = "super pathway")
  
  #############################
  ## MS Method PLOT
  #############################
  ##### order method by mean
  fdata$PLATFORM = factor(feature_data$PLATFORM, levels = platform$PLATFORM, ordered = TRUE)
  ##
  plotB = fdata %>% group_by(PLATFORM) %>% ggplot( aes(x = PLATFORM, y = feature_missing)) +
    geom_boxplot( aes(color = PLATFORM), notch = FALSE) +
    theme( axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(title = "Feature missingness by\nMS method",
         y = "missingness", x = "method")
  
  ### place both plots in a list
  toplot = list(plotA,plotB)
  
  ## return plots to the user
  return(toplot)
}


