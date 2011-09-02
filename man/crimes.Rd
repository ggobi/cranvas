\docType{data}
\name{crimes}
\alias{crimes}
\title{US Crimes data from 2009}
\format{data.frame: 50 obs. of 13 variables}
\source{
  \url{http://www.fbi.gov/about-us/cjis/ucr/ucr}
}
\usage{
data(crimes)
}
\description{
  Counts of different crimes by state across the USA
}
\examples{
library(cranvas)
data(crimes)
crimes$State <- factor(tolower(crimes$State))
qcrimes <- qdata(crimes)
qparallel(data = qcrimes)
}
\keyword{datasets}
