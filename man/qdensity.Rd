\name{qdensity}
\alias{qdensity}
\title{Draw a univariate density plot}
\usage{
qdensity(x, data, binwidth = NULL, main = "", xlim = NULL, ylim = NULL, xlab = NULL, 
    ylab = NULL)
}
\arguments{
  \item{x}{variable name which designates variable
  displayed on the horizontal axis}

  \item{data}{a mutaframe created by \code{\link{qdata}}}

  \item{main}{the main title}

  \item{xlim}{a numeric vector of length 2 (like
  \code{c(x0, x1)}) for x-axis limits; it will be
  calculated from the data limits if not specified
  (\code{NULL}). Note when \code{x0 > x1}, the axis
  direction will be reversed (i.e. from larger values to
  small values)}

  \item{ylim}{y-axis limits; similar to \code{xlim}}

  \item{xlab}{x-axis title}

  \item{ylab}{y-axis title}

  \item{binwidth}{the bin width (\code{range(x) / bins} by
  default)}
}
\description{
Draw a univariate density plot, with a rug plot underneath.
}
\details{
Common interactions are documented in \code{\link{common_key_press}}.
Specific interactions include: Arrow \code{Up}/\code{Down} in-/de-creases
size of points; Arrow \code{Left}/\code{Right} de-/in-creases binwidth for
density; Key \code{Z} toggle zoom on/off (default is off); mouse click &
drag will specify a zoom window.

Note there are two short tickmarks in the plot denoting the binwidth.
}
\examples{
library(cranvas)

### (1) ames housing data
qames <- qdata(ameshousing)

qdensity(saleprice, data = qames)

### (2) tennis data
qtennis <- qdata(tennis)

qdensity(first.serve.pct, data = qtennis)

qdensity(second.serve.pts, data = qtennis)
qdensity(serve.speed, data = qtennis)

record_selector(name, data = qtennis)

### (3) pollen data
if (require("animation")) {
    data(pollen, package = "animation")
    qpollen <- qdata(pollen)
    print(qdensity(RIDGE, data = qpollen))
}

### (4) flea (with colors)
data(flea, package = "tourr")
qflea <- qdata(flea, color = species)

qdensity(tars1, data = qflea)

qdensity(tars2, data = qflea)
qdensity(aede1, data = qflea)
qdensity(aede3, data = qflea)

cranvas_off()
}
\seealso{
Other plots: \code{\link{qbar}}; \code{\link{qboxplot}};
  \code{\link{qhist}}, \code{\link{qspine}};
  \code{\link{qmval}}; \code{\link{qparallel}};
  \code{\link{qtime}}
}
