\name{brush}
\alias{brush}
\alias{brush<-}
\title{Set or query the brush attributes}
\usage{
  brush(data, attr)

  brush(data, attr) <- value
}
\arguments{
  \item{data}{the mutaframe created by \code{\link{qdata}},
  with an attribute \code{Brush}}

  \item{attr}{the name of the brush attribute (a character
  scalar), e.g. \code{style} (the color, linewidth and
  linetype of the brush), \code{color} (the color of the
  objects selected by the brush), \code{size} (the size of
  the selected objects, e.g. line width or size of points);
  if \code{attr} is missing, the whole brush object (as a
  reference object; see \code{\link[methods]{setRefClass}})
  will be returned}

  \item{value}{the value of the brush attribute}
}
\value{
  the brush object or the attribute of the brush; note the
  brush object can be further manipulated with other
  methods -- see examples below.

  set the attribute of the brush
}
\description{
  The brush object in \pkg{cranvas} is essentially an
  environment, and we can manipulate objects in this
  environment.
}
\details{
  The list of attributes in the brush (they can be accessed
  by the \code{$} method):

  \describe{\item{style}{a list containing \code{color},
  \code{linewidth} and \code{linetype} defining the style
  of the brush (rectangle) -- not to be confused with the
  color of the brushed elements} \item{color, size}{the
  color and size of the brushed elements} \item{mode}{the
  brush mode: can be \code{'none'} (default), \code{'and'},
  \code{'or'}, \code{'xor'}, \code{'not'} or
  \code{'complement'}; see \code{\link{mode_selection}} for
  details} \item{identify}{logical: \code{TRUE} (turn on
  the identify mode) or \code{FALSE} (the brush mode)}
  \item{label.gen}{a function to be used to generate the
  labels (based on the identified data) to show in the
  identify mode; the default function just prints the
  identified data as a character string}
  \item{label.color}{the color for the label in the
  identify mode} \item{history.size}{the size of brush
  history, i.e. how many brushing operations to be
  recorded; default to be 30} \item{history.list}{the list
  of indices of the brushed elements; we can go back and
  forth in the brush history according to this list}
  \item{history.index}{the current index of the brush
  history} \item{persistent}{persistent (\code{TRUE}) or
  transient (\code{FALSE}) brushing; in the persistent
  brushing mode, the attributes of the brushed elements
  will be changed permanently} \item{persistent.color}{a
  color vector to store the colors of persistently brushed
  elements} \item{persistent.list}{the persistent brushing
  history (a list of indices of the brushed elements)}
  \item{cursor}{the cursor type (an integer; see
  \code{\link{set_cursor}})}}
}
\examples{
library(cranvas)
qiris <- qdata(head(iris))  # create a mutaframe
brush(qiris)  # the brush object (a reference object)
brush(qiris, "color")
brush(qiris, "color") <- "green"  # set brush color to green

## attach events on the brush
b <- brush(qiris)

# idx is the index of the event; it can be used to stop the
#   listening
idx <- b$colorChanged$connect(function() {
    message("the color of brushed elements was changed to ", 
        b$color)
})
b$color <- "brown"
b$color <- "gold"

b$colorChanged$disconnect(idx)  # disconnect the event

b$style$color <- "red"  # change the color of the brush itself to red
b$style$linewidth <- 3  # the border width to 3

b$mode <- "or"  # brush mode to OR

b$history.size <- 50  # increase history size to 50

b$cursor <- 3L  # cursor type to WaitCursor

b$identify <- TRUE  # turn on identify mode
b$identify <- FALSE  # turn off; i.e. in brushing mode now

b$persistent <- TRUE  # turn on persistent brushing

## redefine label generating function: show row names in
#   the identify mode
b$label.gen <- function(x) {
    paste(rownames(x), collapse = ", ")
}
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link{qdata}}
}
