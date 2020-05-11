#' A Function to estimate power for a binary variable in an imbalanced design
#'
#' This function allows you estimate power for a binary variable given the sample size, effect size, TypeI error. Useful for QC Rmd Report.
#' @param N_case Sample size of cases
#' @param N_control Sample size of controls
#' @param effect effect size
#' @param alpha significance level (Type 1 error)
#' @keywords metabolomics
#' @export
#' @examples
#' eval.power.binary.imbalanced()
eval.power.binary.imbalanced = function(N_case, N_control, effect, alpha) {
  # N_case = N_control = ceiling(N/2)
  N = N_case + N_control
  power_calc = pwr::pwr.t2n.test(n1 = N_case, n2 = N_control, d = effect, sig.level = alpha, power = NULL)
  power <- round(power_calc$power, d=3)
  tmp <- cbind(N, N_case, N_control, effect, alpha, power)
  return(tmp)
}