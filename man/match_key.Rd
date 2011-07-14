\name{match_key}
\alias{match_key}
\title{Match keys from a keyboard event}
\usage{match_key(key, event, logical = TRUE)

}
\description{
This is a simple wrapper function to test if the given keys are
hit in the keyboard event.}
\value{\code{TRUE} for the matched keys, and \code{FALSE} for
those not matched; or an integer vector of the indices of the
matched keys}
\author{Yihui Xie <\url{http://yihui.name}>}
\references{\url{http://doc.qt.nokia.com/latest/qt.html#Key-enum}}
\arguments{\item{key}{a character vector of key names (see the example below)}
\item{event}{the keyboard event}
\item{logical}{whether to return a logical vector or an integer
vector (the latter applies \code{\link[base]{which}} to the
logical vector)}
}
\examples{library(qtbase)
library(qtpaint)
library(cranvas)
key_press = function(layer, event) {
    print(match_key(c("A", "F", "PageUp", "1"), event))
}
s = qscene()
r = qlayer(s, keyPressFun = key_press)
qplotView(scene = s)}







