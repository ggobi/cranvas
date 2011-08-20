\name{focused}
\alias{focused}
\alias{focused<-}
\title{Set or query the focus status}
\usage{
focused(data)

focused(data) <- value
}
\arguments{
  \item{data}{the mutaframe}

  \item{value}{a logical value: whether on focus or not}
}
\value{
  The function \code{\link{focused}} returns a logical
  value: whether the plot corresponding to \code{data} is
  on focus or not
}
\description{
  The plot on top of all the rest of plots is on focus, and
  the corresponding mutaframe is said to be on focus too.
}
\examples{
mf <- qdata(head(iris))

focused(mf)

focused(mf) <- TRUE
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link{qdata}}
}
