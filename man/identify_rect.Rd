\name{identify_rect}
\alias{identify_rect}
\title{Create a rectangle to be used in identifying}
\usage{
  identify_rect(meta)
}
\arguments{
  \item{meta}{the meta data containing at least
  \code{meta$pos} and \code{meta$limits}}
}
\value{
  A Qt rectangle object (see \code{\link[qtbase]{qrect}})
}
\description{
  To identify cases under the mouse, we need a small
  rectangle to look for cases in this rectangle. This
  function creates such a rectangle using \code{meta$pos}
  (mouse position) and \code{meta$limits} (layer limits).
}
\details{
  Currently the size of the rectangle is 1\% of the layer
  limits.
}
\examples{
## see source code of, e.g., qparallel()
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}

