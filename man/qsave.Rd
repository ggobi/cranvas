\name{qsave}
\alias{qsave}
\title{Save the plot to a file}
\usage{
qsave(filename = "Rplot.png", view, width = 480, height = 480)
}
\arguments{
  \item{obj}{the view object (usually returned by a
  plotting function)}

  \item{filename}{the file name (must have an explicit
  extension; see the references for supported image
  formats)}

  \item{width}{the desired width (pixels)}

  \item{height}{the desired height}
}
\value{
  \code{TRUE} if the plot is successfully saved; otherwise
  \code{FALSE}
}
\description{
  This function saves a plot view to a image file like PNG
  or JPEG, etc.
}
\examples{
library(cranvas)
qtennis <- qdata(tennis)
v <- qbar(matches, data = qtennis)
qsave("tennis_bar.png", v, 480, 320)
}
\author{
  Yihui Xie
}
\references{
  Supported image formats:
  \url{http://doc.qt.nokia.com/latest/qimagewriter.html#supportedImageFormats}
}
