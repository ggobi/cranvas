\name{manual_brush}
\alias{manual_brush}
\title{Manually brush the plot via command line}
\usage{
manual_brush(obj, pos, pause = 0)
}
\arguments{
  \item{obj}{the plot object with an attribute \code{meta},
  e.g. returned by \code{\link{qbar}}}

  \item{pos}{the mouse position(s); can be a numeric vector
  of length 2 or a matrix of 2 columns with each row
  representing a mouse position}

  \item{pause}{the time to pause between two successive
  mouse positions (recycled to the same length of
  \code{pos}; each element used after each mouse position)}
}
\value{
  \code{NULL}
}
\description{
  We can brush a plot via command line rather than using
  the mouse.
}
\examples{
library(cranvas)
qnrc <- qdata(nrcstat, color = RegCode)
(o1 <- qbar(RegCode, qnrc))
(o2 <- qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", 
    glyph = "tick", horizontal = FALSE, boxplot = TRUE))

## automatically brush the bar plot
brush(qnrc, "style")$color <- "brown"
l <- attr(o1$view, "meta")$limits
# interpolate some random positions
p <- approx(runif(50, l[1, 1], l[2, 1]), runif(50, l[1, 2], l[2, 2]), n = 500)
manual_brush(o1$view, cbind(p$x, p$y), pause = 0.1)

## can also brush the par-coords plot
manual_brush(o2$view, as.matrix(expand.grid(seq(0, 1, 0.1), (1:4) - 0.1)), 
    pause = 0.2)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
