\name{axis_loc}
\alias{axis_loc}
\title{Calculate pretty locations of axis tick marks}
\usage{
axis_loc(x)
}
\arguments{
  \item{x}{the data vector (either numeric or factor)}
}
\value{
a numeric vector of the axis tick marks locations
}
\description{
The pretty locations are calculated by the function
\code{\link[base]{pretty}}, but the locations that exceed the
range of the data are removed. A special case is the factor: the
locations are 1, 2, ..., up to the number of levels.
}
\examples{
library(cranvas)
axis_loc(1)
axis_loc(c(0, 1))
axis_loc(rnorm(100))
axis_loc(iris$Species)  # a factor, so locations are 1, 2, 3
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
\seealso{
\code{\link[base]{pretty}}
}
