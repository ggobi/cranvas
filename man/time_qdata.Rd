\name{time_qdata}
\alias{time_qdata}
\title{Create data for drawing time plots}
\usage{
time_qdata(regular_qdata, y, timeVar, link)
}
\arguments{
  \item{data}{a data frame for time plot}

  \item{y}{a vertor of all the variable names of interest}

  \item{timeVar}{the variable indicating time. It is used
  to link two mutaframes.}
}
\value{
  A mutaframe of multiple y's
}
\description{
  Create data for drawing time plots
}
\examples{
library(cranvas)
nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
qnasa <- qdata(nasa11)
qnasa1 <- time_qdata(qnasa, c("ts", "ps_tovs", "ca_med"), "TimeIndx")
}
