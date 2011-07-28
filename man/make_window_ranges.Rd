\name{make_window_ranges}
\alias{make_window_ranges}
\title{make the window ranges}
\usage{
  make_window_ranges(dataRanges, xlab = NULL, ylab = NULL,
  xtickmarks = FALSE, ytickmarks = FALSE, main = NULL)
}
\arguments{
  \item{dataRanges}{ranges of the data so a buffer of space
  may be added}

  \item{xlab}{if xlab is replaced with somthing other than
  null, it will be assumed that an axis label will be used}

  \item{ylab}{if ylab is replaced with somthing other than
  null, it will be assumed that an axis label will be used}
}
\value{
  returns a vector of four variables containing xmin, xmax,
  ymin, ymax
}
\description{
  make the window ranges
}
\examples{
make_window_ranges(c(0, 1, 2, 3))
}
\author{
  Barret Schloerke \email{bigbear@iastate.edu}
}
\keyword{hplot}
