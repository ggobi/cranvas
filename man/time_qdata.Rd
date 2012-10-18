\name{time_qdata}
\alias{time_qdata}
\title{Create data for drawing time plots}
\usage{
time_qdata(regular_qdata, y)
}
\arguments{
  \item{data}{a data frame for time plot}

  \item{y}{a vertor of all the variable names of interest}
}
\value{
  A mutaframe of multiple y's
}
\description{
  Create data for drawing time plots
}
\examples{
library(cranvas)
data(nasa)
nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
qnasa <- time_qdata(nasa11, c("ts", "ps_tovs", "ca_med"))
}
