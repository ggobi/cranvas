\name{qbxp}
\alias{qbxp}
\title{Create a boxplot layer}
\usage{
qbxp(parent = NULL, vars = NULL, data, subset = FALSE, 
    at, width, horizontal = FALSE, sister = NULL, ...)
}
\arguments{
  \item{parent}{the parent layer in which to embed this
  boxplot layer}

  \item{vars}{see \code{\link{qboxplot}}}

  \item{data}{the data (a data frame or mutaframe or a list
  containing child elements \code{plot.data} and
  \code{numeric.col})}

  \item{subset}{whether to draw boxplots based on selected
  rows}

  \item{at}{the locations at which to draw the boxplots}

  \item{width}{width(s) of boxes}

  \item{horizontal}{direction of boxplots (\code{TRUE}
  means horizontal)}

  \item{sister}{a sister layer on top of which to put the
  boxplot layer}

  \item{...}{other arguments passed to
  \code{\link[qtpaint]{qlayer}}}
}
\value{
  a layer object
}
\description{
  A ``low-level'' plotting function to create a boxplot
  layer.
}
\examples{
library(cranvas)
library(qtbase)
library(qtpaint)
library(plumbr)

s <- qscene()
r <- qlayer(s)
x <- as.data.frame(matrix(rnorm(100), 20))
m <- qlayer(paintFun = function(layer, painter) {
    qdrawCircle(painter, as.vector(col(x)), as.vector(as.matrix(x)), 
        r = 2, stroke = "gray")
}, limits = qrect(matrix(c(0, 6, range(x)), 2)))  # main layer
b <- qbxp(vars = c("V1", "V3", "V5"), data = x, at = c(1, 
    3, 5), width = 1, sister = m)
r[1, 1] <- b
r[1, 1] <- m
print(qplotView(scene = s))

## when the data argument is a list
s <- qscene()
r <- qlayer(s)
m <- qlayer(paintFun = function(layer, painter) {
    qdrawCircle(painter, as.vector(col(x)), as.vector(as.matrix(x)), 
        r = 2, stroke = "gray")
}, limits = qrect(matrix(c(0, 6, range(x)), 2)))  # main layer
b <- qbxp(data = list(plot.data = x, numeric.col = rep(TRUE, 
    5)), width = 0.6, sister = m)
r[1, 1] <- b
r[1, 1] <- m
print(qplotView(scene = s))
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
