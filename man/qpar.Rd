\name{qpar}
\alias{qpar}
\title{Graphical parameters in cranvas}
\usage{qpar(...)

}
\description{
This function can set or query the graphical parameters in the
\code{cranvas} package.}
\value{the current list of parameters or set new options}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link[graphics]{par}}}
\arguments{\item{...}{options of the form \code{tag = value}}
}
\examples{op = qpar()
qpar(mar = 0.05)  # the degree to extend the plot margin (inner margin)
qpar(op)  # restore}


