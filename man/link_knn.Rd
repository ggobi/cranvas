\name{link_knn}
\alias{link_knn}
\title{k-Nearest neighbor linking}
\usage{
link_knn(mf1, var1, mf2 = NULL, var2 = NULL, ...)
}
\arguments{
  \item{var1}{the variable names or column indices of the
  first mutaframe to be used to calculate distances}

  \item{var2}{(optional) variable names or column indices
  of the second mutaframe}

  \item{mf1}{the first mutaframe}

  \item{mf2}{(optional) the second mutaframe; default
  \code{NULL} means \code{mf1} will be linked to itself}
}
\value{
  Similar to categorical linking (\code{\link{link_cat}}),
  this function also links two mutaframes together (or one
  mutaframe to itself), and id's of listeners are returned.
}
\description{
  When a number of elements in a data are brushed, their
  k-nearest neighbors (based on a certain distance measure)
  are brushed as well.
}
\examples{
## TODO
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
