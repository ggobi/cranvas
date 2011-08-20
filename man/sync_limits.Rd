\name{sync_limits}
\alias{sync_limits}
\title{Sync layer limits}
\usage{
sync_limits(meta, ...)
}
\arguments{
  \item{meta}{the meta data contains a matrix of limits in
  \code{meta$limits}}

  \item{...}{an arbitrary number of layers}
}
\value{
  \code{NULL} (an event is attached on \code{meta$limits}
  so that whenever the limits are changed, the layers will
  be updated using the new limits)
}
\description{
  The limits information is stored in the meta data as
  \code{meta$limits}, of which this function makes use to
  sync the limits of layers.
}
\examples{
## sync_limits(meta, layer1, layer2, layer3)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
