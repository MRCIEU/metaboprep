#' Estimate power for a binary variable
#'
#' This function allows you estimate power for a binary variable given the sample size, effect size, significance threshold.
#'
#' @param N a numeric vector of total study sample size, cases and controls will both be defined as N/2.
#' @param effect a numeric vector of effect size
#' @param alpha a numeric vector of significance thresholds
#'
#' @keywords binary power estimator
#'
#' @importFrom pwr pwr.t2n.test
#' 
#' @return a matrix of parameter inputs and an estimate(s) of power are returned as a matrix
#' 
#' @export
#'
#' @examples
#' eval.power.binary(N = 1000, effect = seq(0.01, 0.3, by = 0.01), alpha = 0.05)
#'
eval.power.binary = function(N, effect, alpha) {
  ## package check
  pkgs = c("pwr")
  for(pkg in pkgs){
    if (!requireNamespace( pkg, quietly = TRUE)) {
        stop(paste0("Package \"", pkg,"\" needed for eval.power.binary() function to work. Please install it."),call. = FALSE)
      }
  }

  ## total sample size
  N_case = N_control = ceiling( N/2 )

  ## power estimate
  power_calc = pwr::pwr.t2n.test(n1 = N_case, n2 = N_control, d = effect, sig.level = alpha, power = NULL)
  
  ## round estimate value
  power <- round(power_calc$power, digits = 3)

  ## data to return
  out <- cbind(N, N_case, N_control, effect, alpha, power)
  return(out)
}
