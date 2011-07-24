\name{common_key_event}
\alias{common_key_press}
\title{Some common processings in the key press and release events}
\usage{common_key_press(layer, event, data, meta)
common_key_release(layer, event, data, meta)

}

\description{
The key press and release events often involve with setting the
selection mode of the \code{\link{brush}}, the alpha transparency,
and deleting selected elements, and so on. These functions
implement these common processes.}
\details{\code{common_key_press}: The keys A, O, X, N and C corresponds to the selection mode AND,
OR, XOR, NOT and COMPLEMENT respectively. Plus (+) and Minus (-)
can increase or decrease the alpha transparency exponentially. The
key Delete will make the selected elements invisible, and F5 makes
all the elements visible.

\code{common_key_release}: In a key release event, we set the selection mode to
\code{'none'}. If PageUp or PageDown was pressed, we show the
brush history.


}
\value{\code{common_key_press}: \code{NULL}

\code{common_key_release}: \code{NULL}


}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link{brush}}}
\arguments{\item{layer}{the layer argument in the event callback}
\item{event}{the event argument in the event callback}
\item{data}{the data created by \code{\link{qdata}}}
\item{meta}{the meta data for a plot}
}
\examples{## see the source code of qbar() or qparallel()}
\alias{common_key_release}
