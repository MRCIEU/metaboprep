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
  ## package check
  pkgs = c("pwr")
  for(pkg in pkgs){
    if (!requireNamespace( pkg, quietly = TRUE)) {
        stop(paste0("Package \"", pkg,"\" needed for eval.power.binary.imbalanced() function to work. Please install it."),call. = FALSE)
      }
  }

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
