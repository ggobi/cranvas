\name{extend_ranges}
\alias{extend_ranges}
\title{Extend the range of data by an amount}
\usage{extend_ranges(x, f = qpar("mar"))

}
\description{Extend the range of data by an amount.}
\details{This is useful for setting a margin in the plot region.}
\value{a vector or a matrix of ranges corresponding to the input
\code{x}}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{x}{the data vector (either the orginal vector or its range)
or a 2x2 matrix which defines the ranges of two axes in two
columns}
\item{f}{the amount to extend the range}
}
\examples{extend_ranges(c(0, 1))
extend_ranges(1:10)

m = matrix(c(c(1, 10), c(5, 8)), nrow = 2)
extend_ranges(m)
extend_ranges(m, c(0.1, 0.2))}





