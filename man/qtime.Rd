\name{qtime}
\alias{qtime}
\title{Draw a time plot..}
\usage{qtime(data, time, y, tgroup, wrap = TRUE, size = 2, 
    alpha = 1, aspect.ratio, ...)

}

\description{Draw a time plot}
\arguments{\item{data}{Mutaframe data to use}
\item{time}{The variable indicating time, which is displayed on the horizontal axis}
\item{y}{The variable displayed on the vertical axis}
\item{tgroup}{The variable to group the time series. Better to be 'year','month',
or other time resolutions. Default to be null.}
\item{wrap}{The switch for wrapping or not when zooming in/out by hitting right 
arrow or left arrow. Default to be TRUE.}
\item{size}{Point size, default to be 2}
\item{alpha}{Transparency level, 1=completely opaque, default to be 1}
\item{aspect.ratio}{Ratio between width and height of the plot}
}
\examples{require(cranvas)

data(nasa)
nasa11 <- subset(nasa, Gridx == 1 & Gridy == 1)
qnasa <- qdata(nasa11)
selected(qnasa)[1] <- TRUE

qtime(qnasa, TimeIndx, ts)
qscatter(qnasa, ts, tsa_tovs)

qtime(qnasa, TimeIndx, c(ts, ps_tovs, ca_med))}










