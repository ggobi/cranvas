\name{qhist2}
\alias{qhist2}
\title{Draw a histogram}
\usage{
  qhist2(x, data, breaks = 30, freq = TRUE, main,
  horizontal = FALSE)
}
\arguments{
  \item{x}{the name of the numeric variable to be used to
  draw the histogram}

  \item{data}{a mutaframe created by \code{\link{qdata}}}

  \item{breaks}{a single number giving the number of bins,
  or a numeric vector giving the breakpoints}

  \item{freq}{draw the frequencies (\code{TRUE}) or
  densities (\code{FALSE})}

  \item{main}{the main title (default to be
  \code{"Histogram of variable name"})}

  \item{horizontal}{whether to draw a horizontal or
  vertical plot}
}
\value{
  A histogram
}
\description{
  Draw an interactive histogram. It supports some common
  keyboard interactions (see
  \code{\link{common_key_press}}) as well as other
  interactions specific to histograms.
}
\details{
  Arrow keys can be used to change the binwidth as well as
  the breakpoints in the histograme. Up and Down can
  increase and decrease the binwidth respectively; Left and
  Right can move the breakpoints of the bins to the left
  (smaller) or right (larger).
}
\examples{
library(cranvas)
data(tennis)
tennis$Matches <- factor(tennis$Matches)
qtennis <- qdata(tennis, fill = Matches, color = Matches)

qhist2(First.Serve.Pct, qtennis)
qbar(Matches, qtennis)

qhist2(First.Serve.Pct, qtennis, horizontal = TRUE)

qhist2(Serve.Speed, qtennis)
qhist2(Serve.Speed, qtennis, freq = FALSE)  # density


## categorical variable linking
data(flea)
qflea <- qdata(flea)
qhist2(tars1, qflea)
qhist2(aede1, qflea)

link_var(qflea) <- "species"
link_type(qflea) <- "self"
## now brush one bar, all rows in the same species will be
#   brushed

## map tars1 to colors
qflea2 <- qdata(flea, color = tars1, fill = tars1)
qhist2(tars1, qflea2)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
