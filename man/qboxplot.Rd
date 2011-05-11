\name{qboxplot}
\alias{qboxplot}
\title{Boxplots..}
\usage{qboxplot(vars, data, at, width, horizontal = FALSE)

}
\description{Boxplots}
\value{NULL}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{vars}{a list of variables, or a formula}
\item{data}{a mutaframe}
\item{at}{the locations of the boxplots}
\item{width}{width(s) of boxes}
\item{horizontal}{horizontal or vertical boxplots}
}
\examples{df = matrix(rnorm(1000), 100)
qboxplot(data = df)
qboxplot(data = df, horizontal = TRUE)}


