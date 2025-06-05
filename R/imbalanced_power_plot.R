#' binary trait imbalanced design power analysis plot
#'
#' This function (1) estimates an informative distribution of effect and power estimates given your datas total sample size, over a distribution of imbalanced sample sizes and (2) returns a summary plot.
#'
#' @param mydata a numeric data matrix with samples in rows and features in columns
#'
#' @keywords binary trait power analysis plot
#'
#' @import ggplot2
#'
#' @return a ggplot2 object
#'
#' @export
#'
#' @examples
#' ex_data = matrix(NA, 1000, 2)
#' imbalanced_power_plot( ex_data )
#'
imbalanced_power_plot = function(mydata){
  ## define local variables
  power <- effect <- N_case <- N_control <- alpha <- NULL


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
  pwrdata = data.table::as.data.table(pwrdata)
  ###
  plotout = pwrdata |>
    ggplot( aes(x=N_case, y=power) ) +
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


#' Estimate power for a binary variable in an imbalanced design
#'
#' This function allows you estimate power for a binary variable given a defined number of case samples, control samples, effect size, and significance threshold.
#'
#'
#' @param N_case a numeric vector of sample size of cases
#' @param N_control a numeric vector of sample size of controls
#' @param effect a numeric vector of effect size
#' @param alpha a numeric vector of significance thresholds
#'
#' @keywords metabolomics
#'
#' @importFrom pwr pwr.t2n.test
#'
#' @return a matrix of paramater inputs and power estimates are returned as a matrix
#'
#' @export
#'
#' @examples
#' eval.power.binary.imbalanced( N_case = 1000,
#'  N_control = 1000,
#'  effect = 0.01,
#'  alpha = 0.05 )
#'
#' eval.power.binary.imbalanced( N_case = c(1000, 2000),
#'  N_control = c(1000, 2000),
#'  effect = 0.01,
#'  alpha = 0.05 )
#'
#'
eval.power.binary.imbalanced = function(N_case, N_control, effect, alpha) {

  ## total sample size
  N = N_case + N_control

  ## estimate power
  power_calc = pwr::pwr.t2n.test(n1 = N_case, n2 = N_control, d = effect, sig.level = alpha, power = NULL)

  ## round the estimate
  power <- round(power_calc$power, digits=3)

  ## values to return
  out <- cbind(N, N_case, N_control, effect, alpha, power)
  return(out)
}
