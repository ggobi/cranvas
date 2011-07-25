\name{qbar}
\alias{qbar}
\title{Create a bar plot}
\usage{qbar(x, data, space = 0.1, main, horizontal = FALSE)

}
\description{Create a bar plot.}
\details{Key events are documented in \code{\link{common_key_press}} and
\code{\link{common_key_release}}. Mouse events mainly include
brushing; as usual, left click to move the brush, and right click
to resize the brush.}
\value{A bar plot}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{x}{a variable name}
\item{data}{a mutaframe created by \code{\link{qdata}}}
\item{space}{the space between bars proportional to the width of bars}
\item{main}{the main title}
\item{horizontal}{\code{TRUE} to draw a horizontal plot or
\code{FALSE} (vertical)}
}
\examples{library(cranvas)
data(nrcstat)
qnrc <- qdata(nrcstat, fill = dscale(nrcstat$Regional.Code, 
    hue_pal()), color = dscale(nrcstat$Regional.Code, hue_pal()))
qbar(Regional.Code, qnrc)
qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", 
    glyph = "tick", horizontal = FALSE, boxplot = TRUE)

qbar(Regional.Code, qnrc, horizontal = TRUE)  # horizontal plot}







