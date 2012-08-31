\docType{data}
\name{pollen}
\alias{pollen}
\title{Synthetic dataset about the geometric features of pollen grains}
\source{
  See the \pkg{animation} package.
}
\description{
  There are 3848 observations on 5 variables. From the 1986
  ASA Data Exposition dataset, made up by David Coleman of
  RCA Labs
}
\examples{
data(pollen)

## some dense points in the center?
plot(pollen[, 1:2], pch = 20, col = rgb(0, 0, 0, 0.1))
}
\keyword{datasets}

