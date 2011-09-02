\docType{data}
\name{nasa}
\alias{nasa}
\title{Spatiotemporal measurements of climate variables}
\format{data.frame: 50 obs. of 13 variables}
\source{
  \url{http://stat-computing.org/dataexpo/2006/}
}
\usage{
data(nasa)
}
\description{
  Monthly measurements from 1995-2000 of temperature,
  pressure ozone and clouds over central America. The data
  was provided for the 2006 ASA Stat Computing and Graphics
  Data Expo competition.
}
\examples{
library(cranvas)
data(nasa)
nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
qnasa <- qdata(nasa11)
qtime(TimeIndx, ~ts, qnasa, shift = c(1, 12))
}
\keyword{datasets}
