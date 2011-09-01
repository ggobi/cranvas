\name{qhist}
\alias{qhist}
\alias{qspine}
\title{Draw a histogram or a spine plot}
\usage{
qhist(x, data = last_data(), bins = 30, binwidth = NULL, 
    freq = TRUE, main = "", horizontal = FALSE, spine = FALSE, 
    xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL)

qspine(x, data = last_data(), ...)
}
\arguments{
  \item{x}{the name of the numeric variable to be used to
  draw the histogram or spine plot}

  \item{bins}{the desired number of bins}

  \item{binwidth}{the bin width (\code{range(x) / bins} by
  default)}

  \item{freq}{draw the frequencies (\code{TRUE}) or
  densities (\code{FALSE}) (only applies to histogram)}

  \item{spine}{if \code{TRUE}, draw a spine plot (bar
  widths proportional to counts instead of being equal)}

  \item{...}{arguments passed to \code{\link{qhist}}}

  \item{data}{a mutaframe created by \code{\link{qdata}}
  (default to be \code{\link{last_data}()}, i.e. the lastly
  used data)}

  \item{main}{the main title}

  \item{horizontal}{\code{TRUE} to draw a horizontal plot
  or \code{FALSE} (vertical)}

  \item{xlim}{a numeric vector of length 2 (like
  \code{c(x0, x1)}) for x-axis limits; it will be
  calculated from the data limits if not specified
  (\code{NULL}). Note when \code{x0 > x1}, the axis
  direction will be reversed (i.e. from larger values to
  small values)}

  \item{ylim}{y-axis limits; similar to \code{xlim}}

  \item{xlab}{x-axis title}

  \item{ylab}{y-axis title}
}
\value{
  A histogram or a spine plot
}
\description{
  Draw an interactive histogram or spine plot based on a
  continuous variable, optionally split by a categorical
  variable. It supports some common keyboard interactions
  (see \code{\link{common_key_press}}) as well as other
  interactions specific to histograms and spine plots.
}
\details{
  The splitting variable is usually specified in
  \code{\link{qdata}} as the \code{color} or \code{border}
  argument; if it is present, each bar in the plot will be
  split into categories.

  Arrow keys can be used to change the binwidth as well as
  the breakpoints in the plot. Up and Down can increase and
  decrease the binwidth respectively; Left and Right can
  move the breakpoints of the bins to the left (smaller) or
  right (larger).

  In the identify mode, the breakpoints of the bin(s) as
  well as counts and proportion of cases in the bin(s) are
  shown as text labels in the plot.

  The function \code{\link{qspine}} is a short-hand version
  of \code{qhist(..., spine = TRUE)}.
}
\examples{
library(cranvas)

#### (1) split histogram by species, and categorical
#   linking
data(flea, package = "tourr")
qflea <- qdata(flea, color = species)

qhist(tars1)
qhist(tars2, freq = FALSE, main = "density of tars2")
qhist(aede1, ylim = c(0, 10), main = "fix y-axis limits")
qhist(head, horizontal = TRUE)
qspine(aede2)
qparallel(~.)  # all variables

id <- link_cat(qflea, "species")  # linking by categorical variable
## now brush one bar, all fleas of the same species will be
#   brushed


## remove linking
remove_listener(qflea, id)


#### (2) tennis data: without a splitting variable
data(tennis)

qtennis <- qdata(tennis, color = "white", border = "black")

qhist(First.Serve.Pct)
qbar(Matches)
qspine(Matches)

qhist(First.Serve.Pct, horizontal = TRUE)

qhist(Serve.Speed)
qhist(Serve.Speed, freq = FALSE)  # density


#### see help(wages) for yet another example
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  Other plots: \code{\link{qbar}}, \code{\link{qboxplot}},
  \code{\link{qmval}}, \code{\link{qparallel}},
  \code{\link{qtime}}
}
