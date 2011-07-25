\name{link_type}
\alias{link_type}
\title{Set or query the type of linking}
\usage{link_type(data)
link_type(data) <- value
}
\description{
Types of linking include hot, cold and self linking. Hot linking
means other plots get updated immediately after the current plot
is brushed; cold linking will not update other plots until they
are on focus; self linking means all the elements in the same
category as the current brushed element(s) will be brushed as
well.}
\value{\code{link_type}: the type of linking

\code{link_type<-}: set the linking type


}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{data}{the mutaframe (typically created by
\code{\link{qdata}}), with an attribute \code{Link}}
\item{value}{the type of linking (possible values are \code{hot},
\code{cold} and \code{self})}
}
\examples{mf <- qdata(iris)
link_type(mf)
link_type(mf) <- "self"
link_type(mf) <- "cold"}



\alias{link_type<-}
