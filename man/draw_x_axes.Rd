\name{draw_x_axes}
\alias{draw_x_axes}
\title{draw x axes...}
\usage{draw_x_axes(plotObj, dataRanges, name, row = 0, col = 0)
}
\description{draw x axes
draws the x axes from the numeric data range}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{plotObj}{Qt plot object to have the layer added to}
\item{dataRanges}{ranges of the data so a buffer of space may be added}
}
\examples{dataRanges <- c(0, 1, 2, 3)
draw_x_axes(make_new_plot(make_window_ranges(dataRanges)), dataRanges)}

