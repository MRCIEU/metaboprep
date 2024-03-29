% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval.power.binary.imbalanced.R
\name{eval.power.binary.imbalanced}
\alias{eval.power.binary.imbalanced}
\title{Estimate power for a binary variable in an imbalanced design}
\usage{
eval.power.binary.imbalanced(N_case, N_control, effect, alpha)
}
\arguments{
\item{N_case}{a numeric vector of sample size of cases}

\item{N_control}{a numeric vector of sample size of controls}

\item{effect}{a numeric vector of effect size}

\item{alpha}{a numeric vector of significance thresholds}
}
\value{
a matrix of paramater inputs and power estimates are returned as a matrix
}
\description{
This function allows you estimate power for a binary variable given a defined number of case samples, control samples, effect size, and significance threshold.
}
\examples{
eval.power.binary.imbalanced( N_case = 1000, 
 N_control = 1000, 
 effect = 0.01, 
 alpha = 0.05 )

eval.power.binary.imbalanced( N_case = c(1000, 2000), 
 N_control = c(1000, 2000), 
 effect = 0.01, 
 alpha = 0.05 )


}
\keyword{metabolomics}
