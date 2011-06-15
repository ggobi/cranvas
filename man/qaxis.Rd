\name{qaxis}
\alias{qaxis}
\title{Create an axis layer}
\usage{qaxis(parent, data, side = 1, at, labels, sister, 
    ...)

}
\description{
This function creates an axis layer which contains tick marks and
labels at given locations.}
\value{a layer object}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link[graphics]{axis}}, \code{\link[qtpaint]{qlayer}}}
\note{The vertical range of the x-axis is [0, 1], and the
horizontal limit of y-axis is [0, 1].}
\arguments{\item{parent}{the parent layer (default to be \code{NULL}, which
means creating an independent layer with no parents, but it can be
added to a parent layer using the approach \code{parent[i, j] <-
child_layer})}
\item{data}{\code{NULL} means to use \code{at} and \code{labels},
otherwise it should be a list containing child elements
\code{xat}, \code{yat}, \code{xlabels} and \code{ylabels}, and it
will override the arguments \code{at} and \code{labels}}
\item{side}{which side to draw the axis (following the convention
of R base graphics, i.e., 1: bottom, 2: left, 3: top, 4: right);
the location of tick marks and labels will automatically adjusted
according \code{side}}
\item{at}{the locations of tick marks}
\item{labels}{the labels of the tick marks}
\item{sister}{a sister layer beside which to draw the axis layer;
the limits of this layer will be used for the axis layer, e.g.,
the x-axis is in the bottom of a main plot layer, so the width of
the axis layer should be the same with the main layer}
\item{...}{other arguments passed to \code{\link[qtpaint]{qlayer}}}
}
\examples{library(cranvas)
library(qtbase)
library(qtpaint)

s = qscene()
r = qlayer(s)
r[1, 1] = qlayer(paintFun = function(layer, painter) {
    qdrawCircle(painter, runif(1000), runif(1000), r = 2)
    qdrawRect(painter, 0, 0, 1, 1)
}, limits = qrect(matrix(c(0, 1, 0, 1), 2)))  # main layer

r[2, 1] = qaxis(side = 1, at = c(0, 0.1, 0.3, 
    0.7, 0.8), sister = r[1, 1])  # x-axis
r[1, 0] = qaxis(side = 2, at = c(0.2, 0.5, 0.6, 
    0.7, 0.9), sister = r[1, 1])  # y-axis
r[0, 1] = qaxis(side = 3, data = list(xat = c(0.1, 
    0.3, 0.7), xlabels = c("a", "b", "c")), sister = r[1, 
    1])  # top x-axis
print(qplotView(scene = s))  # default layout is ugly; tune in r$gridLayout()}















