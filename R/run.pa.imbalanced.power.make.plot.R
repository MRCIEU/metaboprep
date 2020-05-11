#' A Function to simulate power for a spectrum of effect sizes, and generate plots, assuming a presence absence binary trait in an imbalanced design
#'
#' This function allows simulate power for numberous effect estimates for both binary and continuous variables and generate summary plots. Useful for QC Rmd Report.
#' @param mydata Your metabolite data matrix, with samples in rows
#' @keywords metabolomics
#' @export
#' @examples
#' run.pa.imbalanced.power.make.plot()
run.pa.imbalanced.power.make.plot = function(mydata){
  
  ####################################   
  # set N equal to no. in sample
  ####################################
  N = nrow(mydata)       
  half = N/2
  ncases = seq(100, half, 100)
  ncontrol = N - ncases
    
  ####################################
  # simulation paramaters
  ####################################
  # calculate power for different scenarios
  run_parameters <- expand.grid(Ncases = ncases,
                                Ncontrol = ncontrol,
                                n_coeff = 1,
                                #effect = c(0.01,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2),
                                effect = seq(0.1, 0.2, by = 0.01),
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
  ef =seq(0.1, 0.2, by = 0.01)
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
  # pwrdata = bin_power[, c(1,4,5,6,7)]
  #pwrdata$type = as.factor(pwrdata$type)
  
  ####################################
  # plot results
  ####################################
  s = seq(100, half, by = 100)
  pwrdata = tibble::as_tibble(pwrdata)
  ###
  plotout = pwrdata %>% ggplot( aes(x=N_case, y=power) ) +
    geom_line(aes(color = effect), alpha = 0.8, size = 1.5)  +
    scale_linetype_manual(values=c( "dotdash", "solid")) +
    theme_bw() +
    geom_hline(yintercept=0.8, color = "grey50", size = 1) +
    scale_color_brewer(palette="Spectral") +
    labs(y = "power", x = paste0("sample size of N-cases out of a total N of ", N),
         title = "Estimated power across a range of effect sizes",
         color = "effect\nsize",  
         caption = "     ") +
    geom_vline(xintercept = s, linetype="dotted", color = "grey80", size=1.5)
  ##################################
  # return
  ####################################
  return(plotout)
}

