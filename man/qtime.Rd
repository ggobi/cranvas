\name{qtime}
\alias{qtime}
\title{Draw a time plot..}
\usage{qtime(data, time, y, period, group, wrap = TRUE, 
    shift = c(1, 7, 12, 24), size = 2, alpha = 1, asp, main, 
    xlab, ylab, ...)

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
\item{group}{Similar to period, but is used for longitudinal data grouping.}
\item{wrap}{The switch for wrapping or not when zooming in/out by
hitting right arrow or left arrow. Default to be TRUE.}
\item{shift}{Wrapping speed selector. The default possible speeds
are 1,7(for days a week),12(for months),24(for hours).}
\item{size}{Point size, default to be 2.}
\item{alpha}{Transparency level, 1=completely opaque, default to be 1.}
\item{asp}{Ratio between width and height of the plot.}
\item{main}{main title for the plot.}
\item{xlab}{label on horizontal axis, default is name of x variable}
\item{ylab}{label on vertical axis, default is name of y variable}
}
\examples{require(cranvas)

## example 1: NASA temperature data
data(nasa)
nasa11 <- subset(nasa, Gridx == 1 & Gridy == 1)
qnasa <- qdata(nasa11)
selected(qnasa)[1] <- TRUE
print(qtime(qnasa, TimeIndx, ts))
print(qtime(qnasa, TimeIndx, ts, wrap = FALSE))
print(qtime(qnasa, TimeIndx, ts, Year))
print(qtime(qnasa, TimeIndx, ts, Year, wrap = FALSE))
print(qtime(qnasa, TimeIndx, c(ts, ps_tovs, ca_med)))
print(qtime(qnasa, TimeIndx, c(ts, ps_tovs, ca_med), 
    wrap = FALSE))
print(qtime(qnasa, TimeIndx, c(ts, ps_tovs, ca_med), 
    Year))
print(qtime(qnasa, TimeIndx, c(ts, ps_tovs, ca_med), 
    Year, wrap = FALSE))


## example 2: Norway precipitation data
data(norwayprecip)
nwp <- qdata(data.grid.wk)
print(qtime(nwp, time, obs, period = year))
print(qtime(nwp, time, obs, period = year, wrap = FALSE))


## example 3: Remifentanil in the nlme package
require(nlme)
Rem <- qdata(Remifentanil[complete.cases(Remifentanil) & 
    Remifentanil$ID == 1, ])
Remi <- qdata(Remifentanil[complete.cases(Remifentanil), 
    ])
print(qtime(Rem, Time, conc))
print(qtime(Remi, Time, conc, group = ID))
print(qtime(Remi, Time, conc, group = ID, wrap = FALSE))


## example 4: Wages data
data(wages)
wage <- qdata(wages[, 1:3])
wage31 <- qdata(wages[wages$id == 31, 1:3])
print(qtime(wage31, exper, lnw))
print(qtime(wage, exper, lnw, group = id))}







































