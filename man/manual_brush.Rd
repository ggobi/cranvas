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
  mouse positions}
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
data(nrcstat)
qnrc <- qdata(nrcstat, fill = dscale(nrcstat$Regional.Code, 
    hue_pal()), color = dscale(nrcstat$Regional.Code, hue_pal()))
(o1 <- qbar(Regional.Code, qnrc))
(o2 <- qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", 
    glyph = "tick", horizontal = FALSE, boxplot = TRUE))

## automatically brush the bar plot
brush(qnrc, "style")$color <- "brown"
l <- attr(o1, "meta")$limits
p <- approx(runif(50, l[1, 1], l[2, 1]), runif(50, 
    l[1, 2], l[2, 2]), n = 500)  # interpolate some random positions
manual_brush(o1, cbind(p$x, p$y), pause = 0.1)

## can also brush the par-coords plot
manual_brush(o2, as.matrix(expand.grid(seq(0, 1, 0.1), 
    (1:4) - 0.1)), pause = 0.2)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
