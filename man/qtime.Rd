\name{qtime}
\alias{qtime}
\title{Draw a time plot..}
\usage{qtime(time, y, data, size = 2, alpha = 1, aspect.ratio, 
    ...)

}
\description{Draw a time plot}
\arguments{\item{time}{The variable indicating time, which is displayed on the horizontal axis}
\item{y}{The variable displayed on the vertical axis}
\item{data}{Mutaframe data to use}
\item{size}{Point size, default to be 2}
\item{alpha}{Transparency level, 1=completely opaque, default to be 1}
\item{aspect.ratio}{Ratio between width and height of the plot}
}
\examples{require(cranvas)

data(nasa)
nasa11 <- subset(nasa, Gridx == 1 & Gridy == 1)
qnasa <- qdata(nasa11)
selected(qnasa)[1] <- TRUE
qtime(TimeIndx, ts, qnasa)

qscatter(qnasa, ts, tsa_tovs)}








