\docType{data}
\name{crimes}
\alias{crimes}
\title{US Crimes data from 2009}
\format{data.frame: 50 obs. of 12 variables}
\source{
  \url{http://www.fbi.gov/about-us/cjis/ucr/ucr}
}
\description{
  Counts of different crimes by state across the USA
}
\examples{
library(cranvas)
data(crimes)
qcrimes <- qdata(crimes)
qparallel(names(crimes)[-c(1,2)], data=qcrimes)
}
\keyword{datasets}

