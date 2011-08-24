\name{qboxplot}
\alias{qboxplot}
\title{Boxplots for variables in the data or a continuous variable vs a
categorical variable}
\usage{
qboxplot(vars, data = last_data(), at = NULL, width = NULL, 
    horizontal = FALSE)
}
\arguments{
  \item{vars}{a list of variables (a character vector), or
  a formula; a one-sided formula like \code{~ x1 + x2 + x3}
  means to draw side-by-side boxplots for the variables in
  the right hand side, whereas a two-sided formula like
  \code{y ~ x} means boxplots of a continuous \code{y}
  against a categorical \code{x}}

  \item{data}{a mutaframe or data frame; if missing, the
  first argument \code{vars} will be assumed to be the
  data}

  \item{at}{the locations of the boxplots (by default from
  1 to \code{p} where \code{p} is the number of variables
  to plot or the number of levels of the categorical
  variable)}

  \item{width}{width(s) of boxes (do not have to be a same
  value if provided as a numeric vector); by default it is
  about 1/10 of the screen width}

  \item{horizontal}{horizontal or vertical boxplots}
}
\value{
  NULL
}
\description{
  This function can draw side-by-side boxplots for all the
  variables in a data frame or boxplots for a continous
  variable vs a categorical variable.
}
\details{
  The boxplots can respond to changes in the brushed rows,
  i.e., in \code{selected(data)}. When we brush in other
  plots which are based on the same data, there will be
  ``child'' boxplots in this plot showing the distributions
  of the brushed data.
}
\examples{
library(cranvas)

df <- matrix(rnorm(1000), 200)
colnames(df) <- c("V1", "V2", "V3", "V4", "V5")
qboxplot(df)
qboxplot(df, horizontal = TRUE)
qboxplot(df, at = (1:5)^2)  # at different locations
qboxplot(df, width = 0.1 * sample(5))  # different widths
qboxplot(rnorm(100))
qboxplot(Sepal.Length ~ Species, data = iris)

df <- qdata(round(data.frame(x = rnorm(100), y = runif(100), 
    z = rbeta(100, 1, 0.8)), 2))

qboxplot(data = df)
qparallel(~., data = df)  # brush on par-coords and you see little boxplots

data(flea)
qflea <- qdata(flea)
qboxplot(tars1 ~ species, data = qflea)
# qparallel(~ ., data=qflea) #This doesn't work yet, linkng
#   a bit odd

}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
