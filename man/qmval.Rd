\name{qmval}
\alias{qmval}
\title{Draw a missing value plot}
\usage{
  qmval(vars, data, horizontal = TRUE, standardize = TRUE,
    ...)
}
\arguments{
  \item{vars}{variables to show in the plot: a character
  vector of variable names, or a numeric vector of column
  indices, or a two-sided formula like \code{~ x1 + x2 +
  x3} (without the left-hand side); see
  \code{\link{var_names}}}

  \item{...}{arguments passed to \code{\link{qbar}}}

  \item{data}{a mutaframe created by \code{\link{qdata}}}

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

  This plot is built upon the bar plot \code{\link{qbar}}.
}
\examples{
library(cranvas)

## BRFSS data
data(brfss)

qbrfss <- qdata(brfss)
qmval(names(brfss)[40:50], data = qbrfss)
qmval(51:68, data = qbrfss)
qmval(~poorhlth+fruit+greensal, data = qbrfss)

qparallel(100:110, data = qbrfss)

## TAO data
data(tao, package = 'tourr')

qtao <- qdata(tao, data = qtao)
qmval(~., data = qtao)
qmval(~., data = qtao, horizontal = FALSE, standardize = FALSE, main = 'horizontal plot with counts')
qscatter(longitude, latitude, data = qtao)
}
\author{
  Heike Hofmann and Yihui Xie
}
\seealso{
  Other plots: \code{\link{qbar}}, \code{\link{qboxplot}},
  \code{\link{qdensity}}, \code{\link{qhist}},
  \code{\link{qparallel}}, \code{\link{qspine}},
  \code{\link{qtime}}
}

