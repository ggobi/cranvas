\name{Common.meta}
\alias{Common.meta}
\title{Common meta data structure in cranvas}
\description{
  A meta data is created with reference classes
  (\code{\link[methods]{setRefClass}}) in the \pkg{methods}
  and signaling fields in the \pkg{plumbr} package
  (\code{\link[plumbr]{signalingFields}}). It is used to
  make several plotting routines more modularized (e.g. the
  axes and background layers).
}
\details{
  This object only provides a template for the common meta
  data, we have to call
  \code{\link[plumbr]{signalingFields}} in order to
  \emph{really} create a meta object.

  We can use \code{Meta.template$new()} to generate an
  object, which is essentially an environment. This
  environment object is dynamic in the sense that no matter
  where we copy it to or how we modify the components in
  it, all its copies can sync with each other. This free us
  from considering lexical scopes frequently when we do
  interactive graphics, which often involves with global or
  local changes of variables.

  Another advantage is that we can attach events on this
  object. When a component is changed, an event can be
  triggered (see examples below).

  The common meta structure contains the following
  components:

  \describe{\item{alpha}{the alpha transparency}
  \item{main}{the main title} \item{xat, yat}{the tickmarks
  locations on the x and y axis} \item{xlat, ylab}{x and y
  axis titles} \item{xlabels, ylabels}{the tickmarks
  labels} \item{limits}{the limits of the main plot layer
  (it is often the case that some other layers need to sync
  with the main plot layer in limits, so this component is
  very important)} \item{color, border, size}{the real
  color, border and size vectors used to draw the graphical
  elements, which can be different from the corresponding
  columns in the original data} \item{start, pos}{the
  starting position and the current position of the cursor}
  \item{brush.move}{indicating if the brush is in the move
  mode (if not, the brush will be resized when mouse moves}
  \item{brush.size}{the size of the brush rectangle (a
  numeric vector of length 2)} \item{manual.brush}{a
  function to manually brush the plot given the mouse
  position} \item{identified, identify.labels}{the
  identified indices and the text label to draw in the plot
  for the identified cases}}
}
\examples{
library(objectSignals)
My.meta <- setRefClass("My_meta", fields = signalingFields(c(Common.meta, 
    list(horizontal = "logical", xleft = "numeric"))))
meta <- My.meta$new(alpha = 1)
meta$alpha
meta$xat <- 1:5
meta$pos <- c(1, 4)
meta$horizontal <- FALSE

meta2 <- meta
meta2$xat <- 2:6
meta$xat  # meta is changed too

meta$posChanged$connect(function() {
    message("the mouse position is (", paste(meta$pos, collapse = ","), 
        ") now")
})
meta$pos <- c(2, 0)

ls(meta)  # list all the objects in it
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\keyword{internal}
