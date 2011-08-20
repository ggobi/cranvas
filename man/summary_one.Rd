\name{summary_one}
\alias{summary_one}
\title{Give a column-wise scalar summary of the data frame or matrix}
\usage{
summary_one(x, fun = median, ...)
}
\arguments{
  \item{x}{the data frame or matrix to be summarized}

  \item{fun}{the function to be applied to numeric columns}

  \item{...}{other arguments to be passed to \code{fun}}
}
\value{
  a vector corresponding to columns of \code{x}
}
\description{
  For numeric columns, the function specified by \code{fun}
  will be applied (\code{median()} by default); for other
  types of columns, the mode will be caculated (i.e. the
  categories that have the maximum frequencies) and the
  returned value is a single string with the category
  labels separated by \code{', '}.
}
\examples{
summary_one(iris)
summary_one(iris[1:70, ])
summary_one(mtcars)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
