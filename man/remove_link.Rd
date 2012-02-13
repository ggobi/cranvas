\name{remove_link}
\alias{remove_link}
\title{Remove the linking in mutaframes}
\usage{
remove_link(data = last_data(), id = link_id(data))
}
\arguments{
  \item{id}{a character vector of id's of linking listeners
  (can be obtained as the returned value of
  \code{\link{link_cat}} and \code{\link{link_knn}}); by
  default it is a vector of all id's for linking}

  \item{data}{a mutaframe created by \code{\link{qdata}}
  (default to be \code{\link{last_data}()}, i.e. the lastly
  used data)}
}
\value{
  the listeners are removed
}
\description{
  This function removes the linking (listeners) in
  mutaframes.
}
\examples{
## see ?link_cat
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link{link_knn}}, \code{\link{link_cat}}
}
