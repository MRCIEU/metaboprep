#' Nightingale Health metabolomics annotation data set
#'
#' A dataset containing annotation data on Nightingale Health NMR metabolites (lipids)
#'
#' @format A data frame with 233 rows and 7 variables:
#' \describe{
#'   \item{metabolite}{metabolite id}
#'   \item{raw.label}{metabolite name and units}
#'   \item{class}{metabolite annotation class}
#'   \item{subclass}{metabolite annotation subclass}
#'   \item{label}{metabolite name and units}
#'   \item{label.no.units}{metabolite name without units}
#'   \item{derived_features}{a binary yes|no indicating if the metabolite variable is a variable derived of two or more other features in the data set}
#'   ...
#' }
#' @source \url{http://nightingalehealth.com/}
"ng_anno"