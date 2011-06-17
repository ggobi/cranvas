\name{brush_history}
\alias{brush_history}
\title{Create the brush history.}
\usage{brush_history(data, index=selected(data))
}
\description{Create the brush history.
Given the indices of the brushed elements, this function stores
these indices in the \code{\link{brush}} object and changes the
colors of graphical elements permanently (via changing the
\code{.color} column in the data) if in the persistent brushing
mode.}
\details{For the transient brushing: the given indices are stored in the
\code{history.list} component of the brush object. The length of
the list of indices is restricted by the \code{history.size}
component of the brush, i.e., old histories may be removed due to
this size restriction.

For the persistent brushing: the given indices of brushed elements
are stored in the \code{persistent.list} component, and the
current brushing color is also saved to the
\code{persistent.color} component. The colors of brushed elements
will be changed permanently. Finally, the length of the list of
indices is also restricted by the \code{history.size} component of
the brush.

We can use these stored information to redraw the brushed elements
later. See \code{\link{brush}} for detailed explanation of these
components.}
\value{the \code{data} argument will be returned and other
changes occur as side effects}
\author{Yihui Xie <\url{http://yihui.name}>}
\note{The changes occur only if the \code{index} argument is not
empty, or when the \code{data} argument is in the persistent
brushing mode, i.e., when \code{brush(data, 'persistent')} is
\code{TRUE}. In this case, the returned \code{data} will be
different with the one passed in, because the brush object
attached on it has been changed.}
\seealso{\code{\link{brush}}, \code{\link{qdata}}, \code{\link{selected}}}
\arguments{\item{data}{the mutaframe created by \code{\link{qdata}}}
\item{index}{the indices of rows to be stored in history; an
integer vector or a logical vector (will be coerced to integers by
\code{\link[base]{which}}); by default it is
\code{selected(data)}, i.e., the logical vector indicating which
rows are brushed}
}
\examples{library(cranvas)
data(nrcstat)
qnrc = qdata(nrcstat)
selected(qnrc)  # all FALSE by default
selected(qnrc)[1:5] = TRUE  # brush first 5 rows

b = brush(qnrc)  # the brush object
b$history.list  # this list should be empty by default

brush_history(qnrc)  # store currently brushed row indices in history
brush_history(qnrc, c(6, 7, 10))  # another history

b$history.list  # what happened to the brush object?

b$persistent = TRUE  # turn on persistent brushing
b$persistent.list  # this list should be empty by default too
brush_history(qnrc, c(3, 4, 6, 9))  # permanently brush other 4 rows

b$persistent.list  # what happened to the brush object?
b$persistent.color
b$color
b$history.list}

