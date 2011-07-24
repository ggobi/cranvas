\name{qmtext}
\alias{qmtext}
\title{Create a margin text layer}
\usage{qmtext(parent, meta, side = 1, text = "", x, y, 
    cex = 1, sister, ...)

}
\description{
This function is similar to \code{\link[graphics]{mtext}}, which
draws text into the margin of a plot. A slight difference is this
function creates a layer which can be put anywhere in the layout.
We can also create a title layer with this function.}
\details{As in R base graphics, the margin means the bottom, left, top and
right area of the main plot region. This function will adjust the
direction of the text according to the side to which it is drawn,
e.g., the left or right side will make the text vertical.}
\value{a layer object}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link[graphics]{mtext}}, \code{\link[qtpaint]{qlayer}}}
\arguments{\item{parent}{the parent layer (default to be \code{NULL}, which
means creating an independent layer with no parents, but it can be
added to a parent layer using the approach \code{parent[i, j] <-
child_layer})}
\item{meta}{\code{NULL} means to use \code{x} and \code{y}
directly, otherwise it should contain a child element named
\code{limits} which is a matrix and defines the limits of the main
plot region (in the form \code{matrix(c(x0, x1, y0, y1), 2)});
this matrix will be used to calculate the appropriate coordinates
to draw the text so that the text is in the center of the layer;
this argument will override \code{x} and \code{y} if provided}
\item{side}{which side to draw the text (following the convention
of R base graphics); e.g. \code{side = 3} can be used to create
the title layer}
\item{text}{the character string to draw}
\item{x}{the x coordinate}
\item{y}{the y coordinate}
\item{cex}{the expansion factor}
\item{sister}{a sister layer beside which to draw this text layer;
if provided, its limits will be used to calculate \code{x} and \code{y}}
\item{...}{other arguments passed to \code{\link[qtpaint]{qlayer}}}
}
\examples{library(cranvas)
library(qtbase)
library(qtpaint)

s <- qscene()
r <- qlayer(s)  # root layer
m <- qlayer(paintFun = function(layer, painter) {
    qdrawCircle(painter, runif(1000), runif(1000), r = 2)
    qdrawRect(painter, 0, 0, 1, 1)
}, limits = qrect(matrix(c(0, 1, 0, 1), 2)))  # main layer
m1 <- qmtext(text = "x axis title!", side = 1, 
    sister = m)
m2 <- qmtext(text = "y axis title!", side = 2, 
    sister = m)
m3 <- qmtext(text = "THE MAIN TITLE", side = 3, 
    sister = m)
m4 <- qmtext(text = "text on the right margin", 
    side = 4, sister = m)
## note how to arrange these layers appropriately in
#   the margin
r[1, 1] <- m
r[2, 1] <- m1
r[1, 0] <- m2
r[0, 1] <- m3
r[1, 2] <- m4
print(qplotView(scene = s))}




















