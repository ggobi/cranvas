\name{qgrid}
\alias{qgrid}
\title{Create a background grid layer}
\usage{qgrid(parent, data, xat, yat, xlim, ylim, minor = "xy", 
    sister, ...)

}
\description{
A layer with gray background and white grid lines corresponding to
axis tick marks. Minor grid lines are optional and thinner.}
\value{a layer object}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{parent}{the parent layer (default to be \code{NULL}, which
means creating an independent layer with no parents, but it can be
added to a parent layer using the approach \code{parent[i, j] <-
child_layer})}
\item{data}{\code{NULL} means to use \code{xat}, \code{yat},
otherwise it should be a list containing child elements \code{xat}
and \code{yat}, and it will override the next two arguments}
\item{xat}{locations to draw vertical grid lines}
\item{yat}{locations to draw horizontal grid lines}
\item{xlim}{the x-axis limits (\code{c(x0, x1)})}
\item{ylim}{the y-axis limits (\code{c(y0, y1)})}
\item{minor}{defines which minor lines to draw: \code{'x'}: only
on the x-axis; \code{'y'}: only on the y-axis; \code{'xy'}: both x
and y minor grid lines; \code{''}: no minor grid lines}
\item{sister}{the layer beneath which to draw the background grid;
if not \code{NULL}, its limits will be passed to this grid layer
so that their limits can match up with each other}
\item{...}{other arguments passed to \code{\link[qtpaint]{qlayer}}}
}
\examples{library(cranvas)
library(qtbase)
library(qtpaint)

s = qscene()
r = qlayer(s)
m = qlayer(paintFun = function(layer, painter) {
    qdrawCircle(painter, runif(1000), runif(1000), r = 2)
    qdrawRect(painter, 0, 0, 1, 1)
}, limits = qrect(matrix(c(0, 1, 0, 1), 2)))  # main layer
g = qgrid(xat = seq(0, 1, 0.2), yat = seq(0, 1, 
    0.5), sister = m)
r[1, 1] = g  # must add the grid layer FIRST, then the plot layer
r[1, 1] = m
print(qplotView(scene = s))}













