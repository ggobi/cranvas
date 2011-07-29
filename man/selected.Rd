\name{selected}
\alias{selected}
\alias{selected<-}
\title{Set or query the selected (brushed) observations}
\usage{
  selected(data)

  selected(data) <- value
}
\arguments{
  \item{data}{the mutaframe}

  \item{value}{a logical vector of the length
  \code{nrow(data)}, or a vector of integers which will be
  used to create a logical vector with \code{TRUE}
  corresponding to these indicies}
}
\value{
  The function \code{\link{selected}} returns the logical
  vector corresponding to whether the observations are
  selected or not
}
\description{
  The column \code{.brushed} controls which observations
  are being brushed (i.e. those \code{TRUE}'s are
  selected).
}
\examples{
df <- qdata(iris)

selected(df)

selected(df) <- rep(c(TRUE, FALSE), c(10, 140))  # brush the first 10 obs
selected(df)

selected(df) <- 3L  # brush the 3rd row
selected(df)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link{qdata}}
}
