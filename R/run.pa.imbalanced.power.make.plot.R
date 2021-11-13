#' binary trait imbalanced design power analysis plot
#'
#' This function (1) estimates an informative distribution of effect and power estimates given your datas total sample size, over a distribution of imbalanced sample sizes and (2) returns a summary plot.
#'
#' @param mydata a numeric data matrix with samples in rows and features in columns
#'
#' @keywords binary trait power analysis plot
#'
#' @importFrom ggplot2 aes geom_line scale_linetype_manual theme_bw geom_hline scale_color_brewer labs geom_vline
#' @importFrom magrittr %>%
#' 
#' @return a ggplot2 object
#'
#' @export
#'
#' @examples
#' ex_data = matrix(NA, 1000, 2)
#' run.pa.imbalanced.power.make.plot( ex_data )
#'
run.pa.imbalanced.power.make.plot = function(mydata){
  ## define local variables
  power <- effect <- N_case <- N_control <- alpha <- NULL
  ## package check
  pkgs = c("magrittr", "ggplot2")
  for(pkg in pkgs){
    if (!requireNamespace( pkg, quietly = TRUE)) {
        stop(paste0("Package \"", pkg,"\" needed for run.pa.imbalanced.power.make.plot() function to work. Please install it."),call. = FALSE)
      }
  }


  ####################################   
  # set N equal to no. in sample
  ####################################
  N = nrow(mydata)       
  ncases = seq(10, N-10, 10)
  ncontrol = N - ncases
  
  ####################################
  ## identify an informative distribution 
  ## of effect sizes given the sample sizes
  ####################################
  effect_sizes = find.PA.effect.sizes.2.sim(mydata = mydata)
  
  ####################################
  # simulation paramaters
  ####################################
  # calculate power for different scenarios
  run_parameters <- expand.grid(Ncases = ncases,
                                Ncontrol = ncontrol,
                                n_coeff = 1,
                                effect = effect_sizes,
                                alpha = 0.05)
  #################
  ## filter
  #################
  ## ordered value to extract
  o = c()
    for(i in 1:length(ncases) ){
      if(i == 1){
        o = c(o , i )
      } else {
        o = c(o, (length(ncases)*(i-1))+i )
        }
    }

  ##### 
  #ef = seq(0.1, 0.2, by = 0.01)
  ef = effect_sizes
  k = sapply(ef, function(x){  
    w = which(run_parameters$effect == x)
    return(w[o])
    })
  ###
  keep =c()
  for(i in 1:ncol(k)){ keep = c(keep, k[,i])  }
  
  
  run_parameters = run_parameters[keep,]
  
  ####################################
  # calculate power for case-control outcomes
  ####################################
  bin_power <- lapply(1:nrow(run_parameters), function(x){
    eval.power.binary.imbalanced(N_case = run_parameters[x,1], N_control = run_parameters[x,2],
                                 effect = run_parameters[x,4], alpha = run_parameters[x,5])
  })
  
  bin_power <- as.data.frame(do.call(rbind, bin_power))
  bin_power$effect = as.factor(bin_power$effect)
  bin_power$type = "imbalanced_casecontrol"
  
  
  ####################################
  ## merge simulations
  ####################################
  pwrdata = bin_power
  
  ####################################
  # plot results
  ####################################
  #s = seq(0, half, by = 200)
  s = seq(0, N, by = 200)
  pwrdata = tibble::as_tibble(pwrdata)
  ###
  plotout = pwrdata %>% ggplot( aes(x=N_case, y=power) ) +
    geom_line(aes(color = effect), alpha = 0.8, size = 1.5)  +
    scale_linetype_manual(values=c( "dotdash", "solid")) +
    theme_bw() +
    geom_hline(yintercept=0.8, color = "grey50", size = 1) +
    scale_color_brewer(palette="Spectral") +
    labs(y = "power", x = paste0("sample size of N-cases out of a total N of ", N),
         title = "Estimated power for presence/absence imbalanced traits",
         color = "effect\nsize",  
         caption = "     ") +
    geom_vline(xintercept = s, linetype="dotted", color = "grey20", size=0.25)

  ##################################
  # return
  ####################################
  return(plotout)
}

