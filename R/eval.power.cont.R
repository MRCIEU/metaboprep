#' A Function to estimate power for a continuous variable
#'
#' This function allows you estimate power for a continuous variable given the sample size, effect size, TypeI error, and the degrees of freedom. Useful for QC Rmd Report.
#' @param N Sample size
#' @param n_coeff degrees of freedom for numerator
#' @param effect effect size
#' @param alpha significance level (Type 1 error)
#' @keywords metabolomics
#' @export
#' @examples
#' eval.power.cont()
eval.power.cont = function(N, n_coeff, effect, alpha) {
  power_calc = pwr::pwr.f2.test(u = n_coeff, v = N-n_coeff-1, f2 = effect, sig.level = alpha, power = NULL)
  power <- round(power_calc$power, d=3)
  tmp <- cbind(N, effect, alpha, power, n_coeff)
  return(tmp)
}

