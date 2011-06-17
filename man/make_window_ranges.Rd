\name{make_window_ranges}
\alias{make_window_ranges}
\title{make the window ranges...}
\usage{make_window_ranges(dataRanges, xlab, ylab, xtickmarks=FALSE, ytickmarks=FALSE, main)
}
\description{make the window ranges
make the window ranges}
\value{returns a vector of four variables containing xmin, xmax, ymin, ymax}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{dataRanges}{ranges of the data so a buffer of space may be added}
\item{xlab}{if xlab is replaced with somthing other than null, it will be assumed that an axis label will be used}
\item{ylab}{if ylab is replaced with somthing other than null, it will be assumed that an axis label will be used}
}
\examples{make_window_ranges(c(0,1,2,3))}

