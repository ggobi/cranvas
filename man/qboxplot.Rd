\name{qboxplot}
\alias{qboxplot}
\title{Boxplots for a data frame or a continuous variable vs a categorical variable}
\usage{qboxplot(vars, data, at, width, horizontal = FALSE)

}
\description{Boxplots for a data frame or a continuous variable vs a categorical variable.}
\details{This function can draw side-by-side boxplots for all the variables
in a data frame or boxplots for a continous variable vs a
categorical variable.}
\value{NULL}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{vars}{a list of variables, or a formula}
\item{data}{a mutaframe}
\item{at}{the locations of the boxplots}
\item{width}{width(s) of boxes}
\item{horizontal}{horizontal or vertical boxplots}
}
\examples{library(cranvas)

df = matrix(rnorm(1000), 200)
qboxplot(df)
qboxplot(df, horizontal = TRUE)
qboxplot(df, at = (1:5)^2)  # at different locations
qboxplot(df, width = 0.1 * sample(5))  # different widths
qboxplot(rnorm(100))
qboxplot(Sepal.Length ~ Species, data = iris)

df = qdata(round(data.frame(x = rnorm(100), y = runif(100), 
    z = rbeta(100, 1, 0.8)), 2))

qboxplot(data = df)
qparallel(data = df)  # brush on par-coords and you see little boxplots}













