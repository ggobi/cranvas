\name{last_data}
\alias{last_data}
\title{Get the last used data object}
\value{
  The last data object created by \code{\link{qdata}}.
}
\description{
  Since interactive graphics often involves with linking
  based on the same data object, this function provides an
  access to the last used data object, which is often the
  default value for the argument \code{data} in many
  plotting functions in this package.
}
\examples{
library(cranvas)
data(nrcstat)
qnrc <- qdata(nrcstat, color = Regional.Code)
qbar(Regional.Code, data = last_data())
## or simply ignore the data argument
qbar(Regional.Code)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
