\name{qbar}
\alias{qbar}
\title{Create a bar plot}
\usage{qbar(x, data, space = 0.1, main)

}
\description{Create a bar plot.}
\value{A bar plot}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{x}{a variable name}
\item{data}{a mutaframe created by \code{\link{qdata}}}
\item{space}{the space between bars proportional to the width of bars}
\item{main}{the main title}
}
\examples{library(cranvas)
data(nrcstat)
qnrc = qdata(nrcstat, fill = dscale(qnrc$Regional.Code, 
    brewer_pal(type = "qual")))
qbar(Regional.Code, qnrc)
qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", 
    glyph = "tick", horizontal = FALSE, boxplot = TRUE)}





