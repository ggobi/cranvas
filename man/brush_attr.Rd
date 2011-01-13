\name{brush_attr}
\alias{brush_attr}
\title{Get or set brush attributes...}
\usage{brush_attr(data, attr)
}
\description{Get or set brush attributes}
\alias{brush_attr}
\alias{brush_attr<-}
\value{the brush attribute(s) (or as a side effect, change the attribute of
\code{data})}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{data}{the mutaframe created by \code{\link{qmutaframe}},
with an attribute '.brush.attr'}
\item{attr}{the name of the brush attribute, e.g. '.brush.color'
(the color of the brush), '.brushed.color' (the color of the
objects selected by the brush), '.brush.size' (the line width of
the brush), and '.brushed.size' (the size of the selected objects,
e.g. line width or size of points); \code{attr} can be a vector to
access multiple attributes when querying brush attributes (but it
is only allowed to set one attribute at a time)}
}
\examples{qiris = qmutaframe(head(iris))
brush_attr(qiris)  # all attributes
brush_attr(qiris, ".brush.color")
brush_attr(qiris, c(".brushed.color", ".brushed.size"))
brush_attr(qiris, ".brush.color") = "green"  # set brush color to green}




