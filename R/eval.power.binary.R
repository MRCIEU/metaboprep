#' A Function to estimate power for a binary variable
#'
#' This function allows you estimate power for a binary variable given the sample size, effect size, TypeI error. Useful for QC Rmd Report.
#' @param N Sample size
#' @param effect effect size
#' @param alpha significance level (Type 1 error)
#' @keywords binary power estimator
#' @export
#' @examples
#' eval.power.binary()
eval.power.binary = function(N, effect, alpha) {
  N_case = N_control = ceiling( N/2 )
  power_calc = pwr::pwr.t2n.test(n1 = N_case, n2 = N_control, d = effect, sig.level = alpha, power = NULL)
  power <- round(power_calc$power, digits = 3)
  tmp <- cbind(N, N_case, N_control, effect, alpha, power)
  return(tmp)
}

