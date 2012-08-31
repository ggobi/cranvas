\name{check_data}
\alias{check_data}
\title{Check if a data object was created by qdata()}
\usage{
  check_data(data, convert = TRUE)
}
\arguments{
  \item{data}{a data object}

  \item{convert}{whether to convert the input data by
  \code{\link{qdata}}}
}
\value{
  If \code{convert == FALSE}, this function only returns a
  logical value indicating whether the data object was
  created by \code{\link{qdata}}, otherwise it will be
  converted if it was not from \code{qdata(data)}.
}
\description{
  Data objects created by \code{\link{qdata}} has a special
  token. If an object was not created in that way, it can
  be converted by \code{\link{qdata}}.
}
\details{
  This function is designed for developers to check the
  validity of data objects.
}
\examples{
check_data(cbind(x = 1:5, y = 6:10))
check_data(1:8, convert = FALSE); check_data(qdata(mtcars), convert = FALSE)  # TRUE
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}

