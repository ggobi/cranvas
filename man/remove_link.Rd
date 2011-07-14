\name{remove_link}
\alias{remove_link}
\title{Remove the linking between mutaframes}
\usage{remove_link(..., id)

}
\description{
This is the reverse operation to \code{\link{link}}, i.e., this
function removes the linking (listeners) between mutaframes.}
\value{the listeners are removed}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link{link}}}
\arguments{\item{...}{the mutaframes from which the linking will be removed}
\item{id}{a list of the id's of listeners (can be obtained as the
returned value of \code{\link{link}})}
}
\examples{## see ?link}
