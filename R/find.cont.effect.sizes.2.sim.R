#' A Function to estiamte an appropriate distribution of effect sizes to simulate in a power analysis.
#'
#' This function estiamtes an appropriate distribution of effect sizes to simulate in a power analysis.
#' @param mydata Your metabolite data matrix, with samples in rows
#' @keywords metabolomics
#' @export
#' @examples
#' find.cont.effect.sizes.2.sim()
find.cont.effect.sizes.2.sim = function(mydata){
  ####################################   
  # set N equal to no. in sample
  ####################################
  N = nrow(mydata)       
  
  ####################################
  # simulation paramaters
  ####################################
  # calculate power for different scenarios
  run_parameters <- expand.grid(N = N,
                                n_coeff = 1,
                                effect = seq(0.000001, 0.2, by  = 0.00005),
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
  # identify estimate estimate closest 
  # to 0.8
  ####################################
  w_min =  order( abs(con_power$power - 0.2) )[1]
  w_max =  order( abs(con_power$power - max(con_power$power) ) )[1]

  effect_range = c( as.numeric(as.character( con_power$effect[w_min] ) ) , as.numeric( as.character( con_power$effect[w_max] ) ) )
  delta_effect_range = effect_range[2] - effect_range[1]
  effect_steps = round( delta_effect_range/11, d = 5 )
  s = seq(effect_range[1], effect_range[2], effect_steps)
  if(length(s) > 11 ){ s = s[1:11] }
  return( round(s, d = 6) ) 
}