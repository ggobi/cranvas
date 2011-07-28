\name{update_brush_size}
\alias{update_brush_size}
\title{Update the brush size in the mouse move event}
\usage{
  update_brush_size(meta, event)
}
\arguments{
  \item{meta}{the meta data containing the brush
  information (\code{meta$pos}, \code{meta$brush.size},
  \code{meta$brush.move} and \code{meta$start})}

  \item{event}{the event in the callback (if missing, it
  will search in the parent environment \code{sys.frame(1)}
  which is often the callback function)}
}
\value{
  a matrix of the coordinates of the brush rectangle, which
  can be passed to \code{\link[qtbase]{qrect}} and used to
  query the brushed elements by \code{layer$locate()}; as a
  side effect, the brush size is updated, unless it is only
  a single click or the brush is not in the move mode
}
\description{
  The brush size is changed by the differences in two
  successive mouse positions.
}
\details{
  The current mouse position is obtained from
  \code{event$pos()}. If the brush is in the move mode
  (\code{meta$brush.move == TRUE}; often set in a mouse
  click event), the brush size is updated by the
  differences between \code{meta$start} and
  \code{meta$pos}; the former is the starting position of
  the mouse, and the latter is the current position.
}
\examples{
## see source code of qparallel()
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
