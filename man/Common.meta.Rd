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
  We can use \code{Common.meta$new()} to generate an
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

  The common meta data contains the following components:

  \describe{\item{alpha}{the alpha transparency} \item{xat,
  yat}{the tickmarks locations on the x and y axis}
  \item{xlat, ylab}{x and y axis titles} \item{xlabels,
  ylabels}{the tickmarks labels} \item{limits}{the limits
  of the main plot layer (it is often the case that some
  other layers need to sync with the main plot layer in
  limits, so this component is very important)}
  \item{color, border, size}{the real color, border and
  size vectors used to draw the graphical elements, which
  can be different from the corresponding columns in the
  original data} \item{start, pos}{the starting position
  and the current position of the cursor}
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
meta1 <- Common.meta$new(alpha = 1)
meta1$alpha
meta$xat <- 1:5
meta1$pos <- c(1, 4)

meta2 <- meta1
meta2$xat <- 2:6
meta1$xat  # meta1 is changed too

meta1$posChanged$connect(function() {
    message("the mouse position is (", paste(meta1$pos, collapse = ","), 
        ") now")
})
meta1$pos <- c(2, 0)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\keyword{internal}
