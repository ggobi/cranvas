\name{link_var}
\alias{link_var}
\alias{link_var<-}
\title{Set or query the linking variable in a mutaframe}
\usage{
link_var(data)

link_var(data) <- value
}
\arguments{
  \item{data}{the mutaframe (typically created by
  \code{\link{qdata}}), with an attribute \code{Link}}

  \item{value}{the name of the linking variable (or
  \code{NULL} to disable linking); the variable must be a
  factor (i.e. categorical variable)}
}
\value{
\code{\link{link_var}} returns the name of the linking variable
}
\description{
Set or query the linking variable in a mutaframe
}
\examples{
mf <- qdata(head(iris))
link_var(mf)  # NULL
link_var(mf) <- "Species"  # linking by 'Species'
link_var(mf)
link_var(mf) <- NULL  # disable linking
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
\seealso{
\code{\link{qdata}}
}
