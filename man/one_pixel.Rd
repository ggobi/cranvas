\name{one_pixel}
\alias{one_pixel}
\title{Get the relative width and height of one pixel on the screen}
\usage{
one_pixel(painter)
}
\arguments{
  \item{painter}{the painter of a layer}
}
\value{
  A numeric vector of length 2 (width and height).
}
\description{
  This function calculates the relative size of one pixel
  in a layer coordinate system, since it has different
  relative dimensions in different coordinate systems.
}
\examples{
library(qtpaint)
s <- qscene()
qlayer(s, paintFun = function(layer, painter) {
    d <- one_pixel(painter)
    # one pixel segments
    qdrawSegment(painter, 0.1, seq(0, 1, 0.1), 0.1 + d[1], seq(0, 1, 0.1))
    qdrawRect(painter, 0.3, 0.4, 0.3 + d[1], 0.4 + d[2])  # one pixel rectangle
}, limits = qrect(c(0, 1), c(0, 1)))
qplotView(scene = s)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
