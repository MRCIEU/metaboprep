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

