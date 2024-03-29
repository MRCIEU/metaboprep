% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.cont.power.make.plot.R
\name{run.cont.power.make.plot}
\alias{run.cont.power.make.plot}
\title{continuous trait power analysis plot}
\usage{
run.cont.power.make.plot(mydata)
}
\arguments{
\item{mydata}{Your metabolite data matrix, with samples in rows}
}
\value{
a ggplot2 object
}
\description{
This function (1) identifies an informative distribution of effect and power estimates given your datas total sample size and (2) returns a summary plot.
}
\examples{
ex_data = matrix(NA, 1000, 2)
run.cont.power.make.plot( ex_data )

}
\keyword{analysis}
\keyword{continuous}
\keyword{plot}
\keyword{power}
\keyword{trait}
