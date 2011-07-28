\name{draw_grid_with_positions_fun}
\alias{draw_grid_with_positions_fun}
\title{draw grid with qt}
\usage{
  draw_grid_with_positions_fun(plotObj, dataRanges, horiPos
  = NULL, vertPos = NULL, minor.horiPos = NULL,
  minor.vertPos = NULL)
}
\arguments{
  \item{plotObj}{Qt plot object to have the layer added to}

  \item{dataRanges}{ranges of the data so a buffer of space
  may be added}
}
\description{
  draws the grid at given positions
}
\details{
  can be used as part of a recall function to update a
  particular layer
}
\author{
  Barret Schloerke \email{bigbear@iastate.edu}
}
\keyword{hplot}

