\name{extend_ranges}
\alias{extend_ranges}
\title{Extend the range of data by an amount}
\usage{
  extend_ranges(x, f = qpar("mar"))
}
\arguments{
  \item{x}{the data vector (either the orginal vector or
  its range) or an n by 2 matrix which is used to define
  the ranges of two axes in two columns}

  \item{f}{the amount to extend the range (usually a
  scalar; when it is a vector, its length must be 2 (giving
  the amount to extend to the left and right respectively)
  or 4 (extending x-axis and y-axis respectively)}
}
\value{
  a vector or a matrix of ranges corresponding to the input
  \code{x}
}
\description{
  This is useful for setting a margin in the plot region.
}
\examples{
extend_ranges(c(0, 1))
extend_ranges(1:10)

m = matrix(c(c(1,5,10), c(5,7,8)), ncol = 2)
extend_ranges(m)
extend_ranges(m, f = c(.1, .2))  # larger top and right margins
extend_ranges(m, f = c(.2, .2, .1, .1))  # larger horiz margins, small vertical margins
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}

