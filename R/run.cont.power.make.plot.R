#' A Function to simulate power for a spectrum of effect sizes, and generate plots assuming a continusous trait
#'
#' This function allows simulate power for numberous effect estimates for both binary and continuous variables and generate summary plots. Useful for QC Rmd Report.
#' @param mydata Your metabolite data matrix, with samples in rows
#' @keywords metabolomics
#' @export
#' @examples
#' run.cont.power.make.plot()
run.cont.power.make.plot = function(mydata){
  
  ####################################   
  # set N equal to no. in sample
  ####################################
  N = nrow(mydata)       
  N_step_size = N/20

  effect_estimates = find.cont.effect.sizes.2.sim(mydata)
  ####################################
  # simulation paramaters
  ####################################
  # calculate power for different scenarios
  run_parameters <- expand.grid(#N = N*(seq(0.10,1,0.10)),
                                N = seq(10, N, N_step_size ),
                                n_coeff = 1,
                                #effect = c(0.001, 0.005, 0.01,0.025,0.05,0.075,0.1,0.125, 0.15 ),
                                #effect = seq(0.006, 0.024, by  = 0.002),
                                effect = effect_estimates,
                                alpha = 0.05)

  
  ####################################
  # calculate power for continuous outcome
  ####################################
  con_power <- lapply(1:nrow(run_parameters), function(x){
    eval.power.cont(N = run_parameters[x,1], 
                    n_coeff = run_parameters[x,2] , 
                    effect = run_parameters[x,3], 
                    alpha = run_parameters[x,4]
                    )
  })
  
  
  con_power <- as.data.frame(do.call(rbind, con_power))
  con_power$effect = as.factor(con_power$effect)
  con_power$type = "continuous"
  
  ####################################
  ## merge simulations
  ####################################
  pwrdata = con_power[, c(1,2,3,4,6)]
  #pwrdata$type = as.factor(pwrdata$type)
  
  ####################################
  # plot results
  ####################################
  pwrdata = tibble::as_tibble(pwrdata)
  s = seq(0, N, by = N_step_size)
  ###
  plotout = pwrdata %>% ggplot( aes(x=N, y=power) ) +
    geom_line(aes(color = effect), alpha = 0.8, size = 1.5) +
    scale_linetype_manual(values=c( "dotdash", "solid")) +
    theme_bw() +
    geom_hline(yintercept=0.8, color = "grey50", size = 1) +
    scale_color_brewer(palette="Spectral") +
    labs(y = "power", x = "total sample size",
         title = "Estimated power for continuous traits",
         color = "effect\nsize") +
    geom_vline(xintercept = s, linetype="dotted", color = "grey20", size=0.25) 
  
  ####################################
  # return
  ####################################
  return(plotout)
}

