\name{qmtext}
\alias{qmtext}
\title{Create a margin text layer}
\usage{qmtext(parent, meta, side = 1, text = "", x, y, 
    cex = 1, ...)

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
\note{If \code{meta} is not \code{NULL}, it is supposed to be a
reference object, and an event will be attached on
\code{meta$limits} so that the limits of the text layer will sync
with \code{meta$limits} dynamically.}
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
this argument will override \code{x} and \code{y} if provided;
furthermore, the limits of the layer will use \code{meta$limits}
as well}
\item{side}{which side to draw the text (following the convention
of R base graphics); e.g. \code{side = 3} can be used to create
the title layer}
\item{text}{the character string to draw; if \code{meta} is not
\code{NULL}, this argument will take values from \code{meta$xlab}
if \code{side == 1}, \code{meta$ylab} if \code{side == 2}, and
\code{meta$main} if \code{side == 3}}
\item{x}{the x coordinate}
\item{y}{the y coordinate}
\item{cex}{the expansion factor}
\item{...}{other arguments passed to \code{\link[qtpaint]{qlayer}}}
}
\examples{library(cranvas)
library(qtbase)
library(qtpaint)

s <- qscene()
r <- qlayer(s)  # root layer
l <- qrect(matrix(c(0, 1, 0, 1), 2))
m <- qlayer(paintFun = function(layer, painter) {
    qdrawCircle(painter, runif(1000), runif(1000), r = 2)
    qdrawRect(painter, 0, 0, 1, 1)
}, limits = l)  # main layer
m1 <- qmtext(text = "x axis title!", side = 1, 
    x = 0.5, y = 0.5, limits = l)
m2 <- qmtext(text = "y axis title!", side = 2, 
    x = 0.5, y = 0.5, limits = l)
m3 <- qmtext(text = "THE MAIN TITLE", side = 3, 
    x = 0.5, y = 0.5, limits = l)
m4 <- qmtext(text = "text on the right margin", 
    side = 4, x = 0.5, y = 0.5, limits = l)
## note how to arrange these layers appropriately in
#   the margin
r[1, 1] <- m
r[2, 1] <- m1
r[1, 0] <- m2
r[0, 1] <- m3
r[1, 2] <- m4
print(qplotView(scene = s))}





















