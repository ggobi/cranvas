\name{qbar}
\alias{qbar}
\title{Create a bar plot}
\usage{
qbar(x, data = last_data(), space = 0.1, main = "", 
    horizontal = FALSE)
}
\arguments{
  \item{x}{a variable name}

  \item{data}{a mutaframe created by \code{\link{qdata}}}

  \item{space}{the space between bars proportional to the
  width of bars}

  \item{main}{the main title}

  \item{horizontal}{\code{TRUE} to draw a horizontal plot
  or \code{FALSE} (vertical)}
}
\value{
  A bar plot
}
\description{
  Key events are documented in
  \code{\link{common_key_press}} and
  \code{\link{common_key_release}}. Mouse events mainly
  include brushing; as usual, left click to move the brush,
  and right click to resize the brush.
}
\examples{
library(cranvas)
data(nrcstat)
qnrc <- qdata(nrcstat, color = Regional.Code)
qbar(Regional.Code)
qparallel(vars = 13:10, main = "Overview of Rankings", 
    glyph = "tick", horizontal = FALSE, boxplot = TRUE)

qbar(Regional.Code, horizontal = TRUE)  # horizontal plot
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
