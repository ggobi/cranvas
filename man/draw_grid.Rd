\name{draw_grid}
\alias{draw_grid}
\title{draw grid...}
\usage{draw_grid(plotObj, dataRanges, row = 0, col = 0)
}
\description{draw grid
draws the grid from the numeric data ranges}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{plotObj}{Qt plot object to have the layer added to}
\item{dataRanges}{ranges of the data so a buffer of space may be added}
}
\examples{dataRanges <- c(0, 1, 2, 3)
draw_grid(make_new_plot(make_window_ranges(dataRanges)), dataRanges)}

