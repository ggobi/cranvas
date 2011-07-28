\name{draw_brush}
\alias{draw_brush}
\title{Draw the brush rectangle}
\usage{
  draw_brush(painter, brush, meta)
}
\arguments{
  \item{painter}{the painter of the layer on which to draw
  the brush}

  \item{brush}{a brush object created by
  \code{\link{brush}} and attached to a mutaframe; the
  color and line width of the brush are stored in this
  object}

  \item{meta}{the meta data (has a least these two
  components: \code{meta$pos} and \code{meta$brush.size})}
}
\value{
  \code{NULL}
}
\description{
  Draw a rectangle with a spot according to the information
  in the meta data.
}
\examples{
## see the source code of, e.g., qbar() for its usage
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
