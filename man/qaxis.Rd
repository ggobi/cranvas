\name{qaxis}
\alias{qaxis}
\title{Create an axis layer}
\usage{
qaxis(parent = NULL, meta = NULL, side = 1, at = NULL, labels = NULL, 
    ...)
}
\arguments{
  \item{parent}{the parent layer (default to be
  \code{NULL}, which means creating an independent layer
  with no parents, but it can be added to a parent layer
  using the approach \code{parent[i, j] <- child_layer})}

  \item{meta}{\code{NULL} means to use \code{at} and
  \code{labels}, otherwise it should be a list containing
  child elements \code{xat}, \code{yat}, \code{xlabels} and
  \code{ylabels}, and it will override the arguments
  \code{at} and \code{labels}; besides, another child
  element \code{meta$limits} must be present to set the
  limits of the layer}

  \item{side}{which side to draw the axis (following the
  convention of R base graphics, i.e., 1: bottom, 2: left,
  3: top, 4: right); the location of tick marks and labels
  will automatically adjusted according \code{side}}

  \item{at}{the locations of tick marks}

  \item{labels}{the labels of the tick marks}

  \item{...}{other arguments passed to
  \code{\link[qtpaint]{qlayer}}}
}
\value{
  a layer object
}
\description{
  This function creates an axis layer which contains tick
  marks and labels at given locations.
}
\note{
  The vertical range of the x-axis is [0, 1], and the
  horizontal limit of y-axis is [0, 1].

  If \code{meta} is not \code{NULL}, it is supposed to be a
  reference object, and an event will be attached on
  \code{meta$limits} so that the limits of the axis layer
  will sync with \code{meta$limits} dynamically.
}
\examples{
library(cranvas)
library(qtbase)
library(qtpaint)

s <- qscene()
r <- qlayer(s)
r[1, 1] <- qlayer(paintFun = function(layer, painter) {
    qdrawCircle(painter, runif(1000, 0, 10), runif(1000, -5, 3), r = 2)
    qdrawRect(painter, 0, -5, 10, 3)
}, limits = qrect(c(0, 10), c(-5, 3)))  # main layer
## note the vertical limits of x-axis and horizontal limits of y-axis are [0,
## 1]
r[2, 1] <- qaxis(side = 1, at = c(0, 1, 3, 7, 8), limits = qrect(c(0, 
    10), c(0, 1)))  # x-axis
r[1, 0] <- qaxis(side = 2, at = c(-4.5, -1, 0, 1.5, 2.5), limits = qrect(c(0, 
    1), c(-5, 3)))  # y-axis
r[0, 1] <- qaxis(side = 3, meta = list(xat = c(1, 3, 7), xlabels = c("a", 
    "b", "c"), limits = matrix(c(0, 10, -5, 3), 2)))  # top x-axis

fix_dimension(r, row = list(id = c(0, 2), value = c(30, 30)), column = list(id = 0, 
    value = 30))

qplotView(scene = s)

}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link[graphics]{axis}},
  \code{\link[qtpaint]{qlayer}}
}
