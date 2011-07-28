\name{divide_by_maximum}
\alias{divide_by_maximum}
\title{Divide by maximum.}
\usage{
  divide_by_maximum(val, maxVal = val)
}
\arguments{
  \item{val}{value to be scaled}

  \item{maxVal}{vector to be used for finding the maximum
  value}
}
\description{
  Divides a vector by a maximum value of a vector
}
\examples{
divide_by_maximum(1:10)
divide_by_maximum(1:10, 1:20)
}
\author{
  Barret Schloerke \email{bigbear@iastate.edu}
}
\keyword{internal}
