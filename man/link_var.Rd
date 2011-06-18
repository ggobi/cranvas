\name{link_var}
\alias{link_var}
\title{Set or query the linking variable in a mutaframe}
\usage{link_var(data)
link_var(data) <- value
}
\description{Set or query the linking variable in a mutaframe.}
\value{\code{link_var}: the name of the linking variable

\code{link_var<-}: set the linking variable


}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link{qdata}}, \code{\link{link}}}
\arguments{\item{data}{the mutaframe (typically created by
\code{\link{qdata}}), with an attribute \code{Link}}
\item{value}{the name of the linking variable}
}
\examples{mf = qdata(head(iris))
link_var(mf)  # NULL
link_var(mf) = "Species"  # linking by 'Species'
link_var(mf)}



\alias{link_var<-}
