\name{summary_one}
\alias{summary_one}
\title{Give a Column-wise Scalar Summary of the Data Frame or Matrix}
\usage{summary_one(x, fun = median, ...)
}
\description{Give a column-wise scalar summary of the data frame or matrix.}
\details{For numeric columns, the function specified by \code{fun} will be applied
(\code{median()} by default); for other types of columns, the mode will be
caculated (i.e. the categories that have the maximum frequencies) and the
returned value is a single string with the category labels separated by
\code{', '}.}
\value{a vector corresponding to columns of \code{x}}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{x}{the data frame or matrix to be summarized}
\item{fun}{the function to be applied to numeric columns}
\item{...}{other arguments to be passed to \code{fun}}
}
\examples{summary_one(iris)
summary_one(iris[1:70, ])
summary_one(mtcars)}


