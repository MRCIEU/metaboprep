#' identify effect sizes
#'
#' This function estimates an appropriate distribution of effect sizes to simulate in a power analysis.
#'
#' @param mydata Your metabolite data matrix, with samples in rows
#'
#' @keywords power anlysis
#' 
#' @return a vector of effect sizes
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:10, function(x){ rnorm(250, 40, 5) })
#' find.PA.effect.sizes.2.sim(ex_data)
#'
find.PA.effect.sizes.2.sim = function(mydata){
  N = nrow(mydata)       
  half = N/2
  ncases = half
  ncontrol = half
    
  ####################################
  # simulation paramaters
  ####################################
  # calculate power for different scenarios
  run_parameters <- expand.grid(Ncases = ncases,
                                Ncontrol = ncontrol,
                                n_coeff = 1,
                                effect = seq(0.0001, 5, by = 0.005),
                                alpha = 0.05)


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
  # identify estimate estimate closest 
  # to 0.8
  ####################################
  w_min =  order( abs(bin_power$power - 0.2) )[1]
  w_max =  order( abs(bin_power$power - max(bin_power$power) ) )[1]

  effect_range = c( as.numeric(as.character( bin_power$effect[w_min] ) ) , as.numeric( as.character( bin_power$effect[w_max] ) ) )
  delta_effect_range = effect_range[2] - effect_range[1]
  effect_steps = round( delta_effect_range/11, digits = 5 )
  s = seq(effect_range[1], effect_range[2], effect_steps)
  if(length(s) > 11 ){ s = s[1:11] }
  return( round(s, digits = 4) ) 
}