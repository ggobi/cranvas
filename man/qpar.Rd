\name{qpar}
\alias{qpar}
\title{Graphical parameters in cranvas}
\usage{
qpar(...)
}
\arguments{
  \item{...}{options of the form \code{tag = value}}
}
\value{
the current list of parameters or set new options
}
\description{
This function can set or query the graphical parameters in the \pkg{cranvas}
package.
}
\details{
Current options include:

\describe{

\item{cache}{whether to enable cache for graphics layers}

\item{mar}{a percentage for the plot margin}

}
}
\examples{
str(qpar())  # all default parameters
op <- qpar()
qpar(mar = 0.05)  # the degree to extend the plot margin (inner margin)
qpar(op)  # restore
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
\seealso{
\code{\link[graphics]{par}}
}
