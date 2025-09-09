#' continuous trait power analysis plot
#'
#' This function (1) identifies an informative distribution of effect and power estimates given your datas total sample size and (2) returns a summary plot.
#'
#' @param mydata Your metabolite data matrix, with samples in rows
#'
#' @keywords continuous trait power analysis plot
#'
#' @import ggplot2
#'
#' @return a ggplot2 object
#'
#' @export
#'
#' @examples
#' ex_data = matrix(NA, 1000, 2)
#' continuous_power_plot( ex_data )
#'
continuous_power_plot = function(mydata){
  ## define local variables
  N <- N_step_size <- power <- effect <- NULL

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
  run_parameters <- expand.grid(
    N = seq(10, N, N_step_size),
    n_coeff = 1,
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
  #con_power$effect = as.factor(con_power$effect)
  con_power$effect = as.numeric(con_power$effect)
  con_power$type = "continuous"

  ####################################
  ## merge simulations
  ####################################
  pwrdata = con_power[, c(1,2,3,4,6)]
  #pwrdata$type = as.factor(pwrdata$type)

  ####################################
  # plot results
  ####################################
  pwrdata = as.data.frame(pwrdata)
  s = seq(0, N, by = N_step_size)
  ###
  plotout = pwrdata |>
    ggplot( aes(x=N, y=power, color = effect, group = effect) ) +
    geom_line(alpha = 0.8, size = 1.5) +
    scale_linetype_manual(values=c( "dotdash", "solid")) +
    theme_bw() +
    geom_hline(yintercept=0.8, color = "grey50", size = 1) +
    scale_color_gradientn(
      colours = RColorBrewer::brewer.pal(11, "Spectral"),
    ) +
    labs(y = "Power", x = "Total sample size",
         title = "Estimated power for continuous traits",
         color = "Effect\nsize") +
    geom_vline(xintercept = s, linetype="dotted", color = "grey20", size=0.25)

  ####################################
  # return
  ####################################
  return(plotout)
}




#' identify continuos trait effect sizes
#'
#' This function estimates an appropriate distribution of effect sizes to simulate in a continuous trait power analysis.
#'
#' @param mydata Your metabolite data matrix, with samples in rows
#'
#' @keywords power analysis
#'
#' @return a vector of effect sizes
#'
#' @export
#'
#' @examples
#' ex_data = sapply(1:10, function(x){ rnorm(250, 40, 5) })
#' find.cont.effect.sizes.2.sim(ex_data)
#'
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
  effect_steps = round( delta_effect_range/11, digits = 5 )
  s = seq(effect_range[1], effect_range[2], effect_steps)
  if(length(s) > 11 ){ s = s[1:11] }
  return( round(s, digits = 6) )
}




#' estimate power for continuous variable
#'
#' This function estimates power for a continuous variable given the sample size, effect size, significance threshold, and the degrees of freedom.
#'
#' @param N Sample size
#' @param n_coeff degrees of freedom for numerator
#' @param effect effect size
#' @param alpha significance level (Type 1 error)
#'
#' @keywords power continuous
#'
#' @importFrom pwr pwr.f2.test
#'
#' @export
#'
#' @examples
#' eval.power.cont(N = 1000, n_coeff = 1, effect = 0.0025, alpha = 0.05)
#'
eval.power.cont = function(N, n_coeff, effect, alpha) {
  ## package check
  pkgs = c("pwr")
  for(pkg in pkgs){
    if (!requireNamespace( pkg, quietly = TRUE)) {
      stop(paste0("Package \"", pkg,"\" needed for eval.power.cont() function to work. Please install it."),call. = FALSE)
    }
  }
  ## calculate power
  power_calc = pwr::pwr.f2.test(u = n_coeff, v = (N - n_coeff - 1) , f2 = effect, sig.level = alpha, power = NULL)
  power <- round(power_calc$power, digits=3)
  tmp <- cbind(N, effect, alpha, power, n_coeff)
  return(tmp)
}


