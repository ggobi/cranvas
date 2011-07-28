\name{draw_y_axes_with_labels_fun}
\alias{draw_y_axes_with_labels_fun}
\title{draw y axes with qt}
\usage{
  draw_y_axes_with_labels_fun(plotObj, dataRanges,
  axisLabels, labelVertPos, name = NULL)
}
\arguments{
  \item{plotObj}{Qt plot object to have the layer added to}

  \item{dataRanges}{ranges of the data so a buffer of space
  may be added}

  \item{axisLabels}{vector of labels}

  \item{labelVertPos}{vertical position of the axisLabels}
}
\description{
  draws the y axes with the labels and label positions
  given
}
\details{
  can be used as part of a recall function to update a
  particular layer
}
\author{
  Barret Schloerke \email{bigbear@iastate.edu}
}
\keyword{hplot}

