\name{qmval}
\alias{qmval}
\alias{qmval.default}
\alias{qmval.formula}
\alias{qmval.numeric}
\title{Draw a missing value plot}
\usage{
  qmval(vars, data = last_data(), ...)

  \method{qmval}{default} (vars, data = last_data(), space
  = 0.1, main = "", horizontal = TRUE, standardize = TRUE)

  \method{qmval}{numeric} (vars, data = last_data(), ...)

  \method{qmval}{formula} (vars, data = last_data(), ...)
}
\arguments{
  \item{vars}{variables to show in the plot: a character
  vector of variable names, or a numeric vector of column
  indices, or a two-sided formula like \code{~ x1 + x2 +
  x3} (without the left-hand side)}

  \item{...}{arguments passed to the default method}

  \item{data}{a mutaframe created by \code{\link{qdata}}
  (default to be \code{\link{last_data}()}, i.e. the lastly
  used data)}

  \item{space}{the space between bars proportional to the
  width of bars}

  \item{main}{the main title}

  \item{horizontal}{\code{TRUE} to draw a horizontal plot
  or \code{FALSE} (vertical)}

  \item{standardize}{logical: whether to standardize the
  height of each bar to 1}
}
\value{
  A missing value plot
}
\description{
  A missing value plot shows the counts or proportions of
  missing values in each variable. It is essentially a
  stacked bar plot, i.e. a bar plot of variables split by
  the logical vectors of missingness of observations.
}
\details{
  As usual, common interactions are defined in
  \code{\link{common_key_press}}. Brushing on a missing
  value plot has a slightly different meaning with brushing
  other types of plots: if a rectangle is brushed in a
  missing value plot, all rows in the orginal data in which
  the current variable is brushed (i.e. either missing or
  non-missing) are brushed; on the other hand, the brushed
  rows in the original data will also be reflected in the
  missing value plot.
}
\note{
  Unlike most other plots in this package, this plot does
  not fully support linking between all plots. Only one
  missing value plot can link to all other types of plots
  at a time, but all other plots (except other missing
  value plots) can link to all missing value plots.
}
\examples{
library(cranvas)

## BRFSS data
data(brfss)

qbrfss <- qdata(brfss)
qmval(names(brfss)[40:50])
qmval(51:68)
qmval(~POORHLTH + FRUIT + GREENSAL)

qparallel(100:110)

## TAO data
data(tao)

qtao <- qdata(tao)
qmval(~.)
qmval(~., horizontal = FALSE, standardize = FALSE, 
    main = "horizontal plot with counts")
qscatter(longitude, latitude)
}
\author{
  Heike Hofmann and Yihui Xie
}
\seealso{
  Other plots: \code{\link{qbar}}, \code{\link{qboxplot}},
  \code{\link{qhist}}, \code{\link{qparallel}},
  \code{\link{qspine}}, \code{\link{qtime}}
}
