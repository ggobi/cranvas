\docType{data}
\name{tennis}
\alias{tennis}
\title{Dataset of 2006 Australian Open mens tennis matches}
\format{data.frame: 25 obs. of  18 variables}
\source{
  2006 Australian Open mens tennis matches.
}
\description{
  The data contains statistics from the 2006 Australian
  Open mens tennis matches.
}
\examples{
library(cranvas)
data(tennis); qtennis = qdata(tennis)

qscatter(first.serve.pts, second.serve.pts, data = qtennis)
qscatter(matches, sets, data = qtennis)
}
\keyword{datasets}

