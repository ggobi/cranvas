\name{qtime}
\alias{qtime}
\title{Draw a time plot..}
\usage{qtime(data, time, y, period, wrap = TRUE, size = 2, 
    alpha = 1, aspect.ratio, ...)

}

\description{Draw a time plot}
\arguments{\item{data}{Mutaframe data to use}
\item{time}{The variable indicating time, which is displayed on
the horizontal axis}
\item{y}{The variable displayed on the vertical axis}
\item{period}{The variable to group the time series. Better to be
'year','month', or other time resolutions. Default to be
null. When it is not null, the key U and D can be hit to separate
the groups or overlap them together to watch the patterns.}
\item{wrap}{The switch for wrapping or not when zooming in/out by
hitting right arrow or left arrow. Default to be TRUE.}
\item{size}{Point size, default to be 2.}
\item{alpha}{Transparency level, 1=completely opaque, default to be 1.}
\item{aspect.ratio}{Ratio between width and height of the plot.}
}
\examples{require(cranvas)

## example 1: NASA temperature data
data(nasa)
nasa11 <- subset(nasa, Gridx == 1 & Gridy == 1)
qnasa <- qdata(nasa11)
selected(qnasa)[1] <- TRUE
# qtime(qnasa,TimeIndx,ts)
# qtime(qnasa,TimeIndx,ts,wrap=FALSE)
# qtime(qnasa,TimeIndx,ts,Year)
# qtime(qnasa,TimeIndx,ts,Year,wrap=FALSE)
# qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med))
# qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med),wrap=FALSE)
# qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med),Year)
#
#   qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med),Year,wrap=FALSE)


## example 2: Norway precipitation data
data(norwayprecip)
nwp <- qdata(data.grid.wk)
# qtime(nwp,time,obs,year)
# qtime(nwp,time,obs,year,wrap=FALSE)


## example 3: Remifentanil in the nlme package
require(nlme)
Rem <- qdata(Remifentanil[complete.cases(Remifentanil) & 
    Remifentanil$ID == 1, ])
Remi <- qdata(Remifentanil[complete.cases(Remifentanil), 
    ])
# qtime(Rem,Time,conc)
# qtime(Remi,Time,conc,ID)


## example 4: Wages data
data(wages)
wage <- qdata(wages[, 1:3])
wage31 <- qdata(wages[wages$id == 31, 1:3])
# qtime(wage31,exper,lnw)
# qtime(wage,exper,lnw,id)}






































