\name{qmval}
\alias{qmval}
\title{Interactive Missing Value Plot}
\usage{
  qmval(data, vars, main, varmax = 20, ...)
}
\arguments{
  \item{data}{a mutaframe which is typically built upon a
  data frame along with several row attributes}

  \item{...}{}
}
\value{
  NULL
}
\description{
  An interactive missing value plot.
}
\examples{
require(plyr)
library(cranvas)

# BRFSS data
data(brfss)
qbrfss <- qdata(brfss)
qmval(qbrfss, vars = 1:50)
qparallel(qbrfss, vars = 1:3)

# TAO data
data(tao)
qtao <- qdata(tao)
qmval(qtao)
qscatter(longitude, latitude, qtao)
}
\author{
  Heike Hofmann
}
