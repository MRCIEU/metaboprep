#' Nightingale Health metabolomics annotation data set
#'
#' A dataset containing annotation data on Nightingale Health NMR metabolites (lipids), compiled from public resources with additional annotation made here. Some metabolites are repeated in the ng_anno data frame as we have observed that Nightingale Health has changed the spellings a few times. Our code and this data frame attempts to capture all possible spellings that we have observed. We note that as Nightingale Health continues to add new metabolites, updates names, modifies names, or changes spellings the automation of the annotation may fail for some features|metabolites.
#'
#' @format A data frame with 291 rows and 7 variables:
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