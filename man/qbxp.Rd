\name{qbxp}
\alias{qbxp}
\title{Create a boxplot layer}
\usage{
qbxp(data, meta, subset = FALSE, ...)
}
\arguments{
  \item{meta}{the meta data}

  \item{subset}{whether to draw boxplots based on selected
  rows}

  \item{...}{other arguments passed to
  \code{\link[qtpaint]{qlayer}}}

  \item{data}{a mutaframe created by \code{\link{qdata}}}
}
\value{
  a layer object
}
\description{
  A ``low-level'' plotting function to create a boxplot
  layer.
}
\examples{
## see source code of qboxplot()
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
