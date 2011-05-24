\name{brush}
\alias{brush}
\title{Set or query the brush attributes}
\usage{brush(data, attr)
brush(data, attr) <- value
}
\description{Set or query the brush attributes.}
\value{\code{brush}: the brush object or the attribute of the brush; note the
brush object can be further manipulated with other methods -- see
examples below

\code{brush<-}: set the attribute of the brush


}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{data}{the mutaframe created by \code{\link{qdata}},
with an attribute \code{Brush}}
\item{attr}{the name of the brush attribute (a character scalar),
e.g. \code{style} (the color, size and linetype of the brush),
\code{color} (the color of the objects selected by the brush),
\code{size} (the size of the selected objects, e.g. line width or
size of points); if \code{attr} is missing, the brush object will
be returned}
\item{value}{the value of the brush attribute}
}
\examples{qiris = qdata(head(iris))
brush(qiris)  # all attributes
brush(qiris, "color")
brush(qiris, "color") = "green"  # set brush color to green

## attach events on the brush
b = brush(qiris)

# idx is the index of the event; it can be used to stop
#   the listening
idx = b$colorChanged$connect(function() {
    message("the color of brushed elements was changed to ", 
        b$color)
})
b$color = "brown"
b$color = "gold"

# disconnect the event
b$colorChanged$disconnect(idx)}















\alias{brush<-}
