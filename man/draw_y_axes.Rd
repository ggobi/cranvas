\name{draw_y_axes}
\alias{draw_y_axes}
\title{draw y axes...}
\usage{draw_y_axes(plotObj, dataRanges, name, row = 0, col = 0)

}
\description{draw y axes
draws the y axes from the numeric data range}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{plotObj}{Qt plot object to have the layer added to}
\item{dataRanges}{ranges of the data so a buffer of space may be added}
}
\examples{dataRanges <- c(0, 1, 2, 3)
draw_y_axes(make_new_plot(make_window_ranges(dataRanges)), 
    dataRanges)}

