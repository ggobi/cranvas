\name{match_key}
\alias{match_key}
\title{Match keys from a keyboard event}
\usage{
  match_key(key, event)
}
\arguments{
  \item{key}{a character vector of key names (see the
  example below)}

  \item{event}{the keyboard event (if missing, the default
  value comes from the \code{event} argument of the parent
  function (\code{sys.frame(1)}), so if this function is
  called under a standard callback of a layer event, we can
  leave this argument blank)}
}
\value{
  \code{TRUE} for the matched keys, and \code{FALSE} for
  those not matched
}
\description{
  This is a simple wrapper function to test if the given
  keys are hit in the keyboard event.
}
\examples{
library(qtbase)
library(qtpaint)
library(cranvas)
key_press <- function(layer, event) {
    print(match_key(c("A", "F", "PageUp", "1"), event))
}
s <- qscene()
r <- qlayer(s, keyPressFun = key_press)
qplotView(scene = s)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\references{
  \url{http://doc.qt.nokia.com/latest/qt.html#Key-enum}
}
