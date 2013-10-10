\name{qboxplot}
\alias{qboxplot}
\title{Draw boxplots for several variables in the data or a continuous
variable vs a categorical variable}
\usage{
qboxplot(vars, data, at = NULL, width = NULL, horizontal = FALSE, main = "", 
    xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, points = FALSE)
}
\arguments{
  \item{vars}{a list of variables (a character vector), or
  a formula; a one-sided formula like \code{~ x1 + x2 + x3}
  means to draw side-by-side boxplots for the variables in
  the right hand side, whereas a two-sided formula like
  \code{y ~ x} means boxplots of a continuous \code{y}
  against a categorical \code{x}}

  \item{at}{the locations of the boxplots (by default from
  1 to \code{p} where \code{p} is the number of variables
  to plot or the number of levels of the categorical
  variable)}

  \item{width}{width(s) of boxes (do not have to be a same
  value if provided as a numeric vector); by default it is
  about 1/10 of the screen width}

  \item{horizontal}{horizontal or vertical boxplots}

  \item{points}{whether to add data points to the boxplot}

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
}
\value{
  A boxplot
}
\description{
  This function can draw side-by-side boxplots for all the
  variables in a data frame or boxplots for a continous
  variable vs a categorical variable.
}
\details{
  Common interactions are documented in
  \code{\link{common_key_press}}. Note boxplots also
  supports brushing and can respond to brushing in other
  plots. When we brush in other plots which are based on
  the same data, there will be ``child'' boxplots in this
  plot showing the distributions of the brushed data.
}
\examples{
library(cranvas)

### (1) some test cases
df <- qdata(as.data.frame(matrix(rnorm(1000), 200)))
qboxplot(~., df)
qboxplot(~., df, horizontal = TRUE)
qboxplot(~., df, at = (1:5)^2)  # at different locations
qboxplot(~., df, width = 0.1 * sample(5))  # different widths


### (2) flea data
data(flea, package = "tourr")
qflea <- qdata(flea)

## continuous vs categorical variable
qboxplot(tars1 ~ species, data = qflea)

## side-by-side boxplots for several continuous variables
qboxplot(~aede1 + aede2 + aede3, data = qflea)

## brush on other plots and we see little boxplots
qparallel(~., data = qflea)

qscatter(tars1, tars2, data = qflea)


## we can turn on the point layer too
qboxplot(tars1 ~ species, data = qflea, points = TRUE)

## show points with colors
qflea2 <- qdata(flea, color = species)
qboxplot(~aede1 + aede2 + aede3, data = qflea2, points = TRUE)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  Other plots: \code{\link{qbar}}, \code{\link{qdensity}},
  \code{\link{qhist}}, \code{\link{qmval}},
  \code{\link{qparallel}}, \code{\link{qspine}},
  \code{\link{qtime}}
}
