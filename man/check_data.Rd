\name{check_data}
\alias{check_data}
\title{Check if a data object was created by qdata()}
\usage{
check_data(data)
}
\arguments{
  \item{data}{a data object}
}
\value{
  If \code{data} was created by \code{\link{qdata}}, it
  will be returned untouched, otherwise \code{qdata(data)}
  is returned.
}
\description{
  Data objects created by \code{\link{qdata}} has a special
  token. If an object was not created in that way, it will
  be converted by \code{\link{qdata}}.
}
\details{
  This function is designed for developers to check the
  validity of data objects.
}
\examples{
check_data(cbind(x = 1:5, y = 6:10))
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
