#' A Function to simulate power for a spectrum of effect sizes, and generate plots
#'
#' This function allows simulate power for numberous effect estimates for both binary and continuous variables and generate summary plots. Useful for QC Rmd Report.
#' @param mydata Your metabolite data matrix, with samples in rows
#' @keywords metabolomics
#' @export
#' @examples
#' run.power.make.plot()
run.power.make.plot = function(mydata){
  
  ####################################   
  # set N equal to no. in sample
  ####################################
  N = nrow(mydata)       
  
  ####################################
  # simulation paramaters
  ####################################
  # calculate power for different scenarios
  run_parameters <- expand.grid(N = N*(seq(0.10,1,0.10)),
                                n_coeff = 1,
                                effect = c(0.01,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2),
                                alpha = 0.05)
  
  
  ####################################
  # calculate power for case-control outcomes
  ####################################
  bin_power <- lapply(1:nrow(run_parameters), function(x){
    eval.power.binary(N = run_parameters[x,1], effect = run_parameters[x,3], alpha = run_parameters[x,4])
  })
  
  bin_power <- as.data.frame(do.call(rbind, bin_power))
  bin_power$effect = as.factor(bin_power$effect)
  bin_power$type = "casecontrol"
  
  
  ####################################
  # calculate power for continuous outcome
  ####################################
  con_power <- lapply(1:nrow(run_parameters), function(x){
    eval.power.cont(N= run_parameters[x,1], 
                    n_coeff = run_parameters[x,2], 
                    effect = run_parameters[x,3], 
                    alpha = run_parameters[x,4])
  })
  
  
  con_power <- as.data.frame(do.call(rbind, con_power))
  con_power$effect = as.factor(con_power$effect)
  con_power$type = "continuous"
  
  ####################################
  ## merge simulations
  ####################################
  pwrdata = rbind(bin_power[, c(1,4,5,6,7)], con_power[, c(1,2,3,4,6)] )
  pwrdata$type = as.factor(pwrdata$type)
  
  ####################################
  # plot results
  ####################################
  pwrdata = tibble::as_tibble(pwrdata)
  ###
  plotout = pwrdata %>% ggplot( aes(x=N, y=power) ) +
    geom_line(aes(color = effect, linetype = type), alpha = 0.8, size = 1.5) +
    scale_linetype_manual(values=c( "dotdash", "solid")) +
    theme_bw() +
    geom_hline(yintercept=0.8, color = "grey50", size = 1) +
    scale_color_brewer(palette="Spectral") +
    labs(y = "power", x = "total sample size",
         title = "Estimated power across a range of effect sizes",
         color = "effect\nsize", linetype = "outcome\ntype", 
         caption = "             Note: In case/control analysis N_cases = N_controls = N_total / 2")
  ####################################
  # return
  ####################################
  return(plotout)
}


