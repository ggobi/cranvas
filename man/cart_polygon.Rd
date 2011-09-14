\name{cart_polygon}
\alias{cart_polygon}
\title{Calculate coordinates of transformed polygons to make cartograms}
\usage{
cart_polygon(x, y, size, nrow = 100, ncol = 100, ...)
}
\arguments{
  \item{x,y}{the x and y coordinates of original polygons
  (polygons are separated by \code{NA}'s)}

  \item{size}{the size vector of polygons (length must be
  equal to the number of polygons, i.e. the number of
  \code{NA}'s plus 1)}

  \item{nrow,ncol}{numbers to define a grid for the
  cartogram algorithm (see references in \pkg{Rcartogram});
  this can affect the convergence and speed of the
  algorithm, so may need to be adjusted for a few times}

  \item{...}{other arguments passed to
  \code{\link[Rcartogram]{cartogram}}}
}
\value{
  A data frame of two columns \code{x} and \code{y}
  (coordinates of transformed polygons)
}
\description{
  Based on the given sizes of polygons, this function
  calculates the transformed coordinates using the
  \pkg{Rcartogram} package.
}
\examples{
library(cranvas)

## a simple example: two polygons of size 1 and 5
#   respectively
theta <- seq(0, 2 * pi, length = 100)
x <- c(cos(theta), NA, cos(theta) + 2)
y <- c(sin(theta), NA, sin(theta) + 1)
plot(x, y, type = "n", xlim = c(-2, 5), ylim = c(-2, 
    3))
polygon(x, y)
text(c(0, 2), c(0, 1), c(1, 5))  # original polygons

res <- cart_polygon(x, y, c(1, 5), nrow = 100, ncol = 100)
polygon(res$x, res$y, border = "red", lty = 2, lwd = 2)  # transformed polygons


## see map_qdata() for examples of real maps
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
