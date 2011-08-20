\name{self_link}
\alias{self_link}
\title{Update a mutaframe by the linking on itself through a categorical
variable}
\usage{
self_link(data)
}
\arguments{
  \item{data}{a mutaframe created by \code{\link{qdata}}}
}
\value{
  the \code{.brushed} column in the mutaframe is updated
}
\description{
  If the linking type is \code{self} on a mutaframe, and
  the linking variable has been specified, this function
  will brush all the rows which are in the same category or
  categories of the current brushed elements.
}
\examples{
mf <- qdata(iris)
selected(mf)[1] <- TRUE  # brush the 1st row
selected(mf)

link_var(mf) <- "Species"
link_type(mf) <- "self"
self_link(mf)  # all the first 50 rows are brushed
selected(mf)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link{link_var}}, \code{\link{link_type}}
}
