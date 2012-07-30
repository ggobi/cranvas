\name{qgrid}
\alias{qgrid}
\title{Create a background grid layer}
\usage{
qgrid(parent = NULL, meta = NULL, xat, yat, xlim, ylim, minor = "xy", ...)
}
\arguments{
  \item{parent}{the parent layer (default to be
  \code{NULL}, which means creating an independent layer
  with no parents, but it can be added to a parent layer
  using the approach \code{parent[i, j] <- child_layer})}

  \item{meta}{\code{NULL} means to use \code{xat},
  \code{yat}, otherwise it should be an object containing
  child elements \code{xat} and \code{yat}, and it will
  override the next two arguments; besides, the limits of
  the layer will be set to \code{meta$limits} so this
  element should be present as well}

  \item{xat}{locations to draw vertical grid lines}

  \item{yat}{locations to draw horizontal grid lines}

  \item{xlim}{the x-axis limits (\code{c(x0, x1)})}

  \item{ylim}{the y-axis limits (\code{c(y0, y1)})}

  \item{minor}{defines which minor lines to draw:
  \code{'x'}: only on the x-axis; \code{'y'}: only on the
  y-axis; \code{'xy'}: both x and y minor grid lines;
  \code{''}: no minor grid lines}

  \item{...}{other arguments passed to
  \code{\link[qtpaint]{qlayer}}}
}
\value{
  a layer object
}
\description{
  A layer with gray background and white grid lines
  corresponding to axis tick marks. Minor grid lines are
  optional and thinner.
}
\note{
  If \code{meta} is not \code{NULL}, it is supposed to be a
  reference object, and an event will be attached on
  \code{meta$limits} so that the limits of the grid layer
  will sync with \code{meta$limits} dynamically.
}
\examples{
library(cranvas)
library(qtbase)
library(qtpaint)

s <- qscene()
r <- qlayer(s)
l <- qrect(matrix(c(0, 1, 0, 1), 2))
m <- qlayer(paintFun = function(layer, painter) {
    qdrawCircle(painter, runif(1000), runif(1000), r = 2)
    qdrawRect(painter, 0, 0, 1, 1)
}, limits = l)  # main layer
g <- qgrid(xat = seq(0, 1, 0.2), yat = seq(0, 1, 0.5), xlim = c(0, 1), ylim = c(0, 
    1), limits = l)
r[1, 1] <- g  # must add the grid layer FIRST, then the plot layer
r[1, 1] <- m
print(qplotView(scene = s))
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link[graphics]{grid}},
  \code{\link[qtpaint]{qlayer}}
}
