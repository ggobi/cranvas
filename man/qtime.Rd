\name{qtime}
\alias{qtime}
\title{Draw a time plot}
\usage{
qtime(time, y, data, period = NULL, group = NULL, 
    wrap = TRUE, shift = c(1, 7, 12, 24), size = 2, alpha = 1, 
    asp = NULL, main = NULL, xlab = NULL, ylab = NULL, ...)
}
\arguments{
  \item{time}{The variable indicating time, which is
  displayed on the horizontal axis}

  \item{y}{The variable(s) displayed on the vertical axis.
  It must be a formula with only right hand side at the
  moment. See examples.}

  \item{data}{Mutaframe data to use}

  \item{period}{The variable to group the time series.
  Better to be 'year','month', or other time resolutions.
  Default to be null. When it is not null, the key U and D
  can be hit to separate the groups or overlap them
  together to watch the patterns.}

  \item{group}{Similar to period, but is used for
  longitudinal data grouping.}

  \item{wrap}{The switch for wrapping or not when zooming
  in/out by hitting right arrow or left arrow. Default to
  be TRUE.}

  \item{shift}{Wrapping speed selector. The default
  possible speeds are 1,7(for days a week),12(for
  months),24(for hours).}

  \item{size}{Point size, default to be 2.}

  \item{alpha}{Transparency level, 1=completely opaque,
  default to be 1.}

  \item{asp}{Ratio between width and height of the plot.}

  \item{main}{main title for the plot.}

  \item{xlab}{label on horizontal axis, default is name of
  x variable}

  \item{ylab}{label on vertical axis, default is name of y
  variable}
}
\description{
  Draw a time-series plot.
}
\details{
  Arrow up/down: in-/de-crease size of points. Arrow
  left/right: wrap the time series when wrap=TRUE, while
  zoom in/out with the center of the last clicked dot when
  wrap=FALSE. Shift + right: when wrap=TRUE, the time
  series will be folded directly to the width of maximal
  value in argument shift. Shift + left: time series will
  be backed to the original xaxis position, no matter wrap
  is TRUE or FALSE. Key '+'/'-': de-/in-crease alpha level
  (starts at alpha=1 by default). Key 'u'/'d': separate/mix
  the series groups by shifting them up and down. Shift +
  'u'/'d': for multivariate y's, separate/mix them by
  shifting up and down. Key 'g': change the wrapping speed
  circularly in the values of parameter 'shift'.
}
\examples{
library(cranvas)

## example 1: NASA temperature data
data(nasa)
nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
qnasa <- qdata(nasa11)
print(qtime(TimeIndx, ~ts, qnasa, shift = c(1, 12)))
print(qtime(TimeIndx, ~ts, qnasa, wrap = FALSE))
print(qtime(TimeIndx, ~ts, qnasa, Year, shift = 1))
print(qtime(TimeIndx, ~ts, qnasa, Year, wrap = FALSE))
print(qtime(TimeIndx, ~ts + ps_tovs + ca_med, qnasa, 
    shift = c(1, 12)))
print(qtime(TimeIndx, ~ts + ps_tovs + ca_med, qnasa, 
    wrap = FALSE))
print(qtime(TimeIndx, ~ts + ps_tovs + ca_med, qnasa, 
    Year))
print(qtime(TimeIndx, ~ts + ps_tovs + ca_med, qnasa, 
    Year, wrap = FALSE))

library(reshape)
nasaTsCa <- nasa11[, c(6, 9, 14)]
nasaTsCa[, 2:3] <- rescaler(nasaTsCa[, 2:3])
nasaTsCa <- melt(nasaTsCa, 1)
qnasaTsCa <- qdata(nasaTsCa)
print(qtime(TimeIndx, ~value, qnasaTsCa, group = variable, 
    shift = c(1, 12)))


## example 2: Remifentanil in the nlme package
require(nlme)
Rem <- qdata(Remifentanil[complete.cases(Remifentanil) & 
    Remifentanil$ID == 1, ])
Remi <- Remifentanil[complete.cases(Remifentanil), 
    ]
Remi$ID <- factor(Remi$ID)
qRemi <- qdata(Remi)
print(qtime(Time, ~conc, Rem))
print(qtime(Time, ~conc, qRemi, group = ID))
print(qtime(Time, ~conc, qRemi, group = ID, wrap = FALSE))

# for categorical brushing self-link dataset by ID:
link_var(qRemi) <- "ID"  # ON
link_type(qRemi) <- "self"

link_var(qRemi) <- NULL  # OFF


## example 3: Wages
data(wages)
wage <- qdata(wages[as.integer(as.character(wages$id)) < 
    2000, 1:3])
print(qtime(exper, ~lnw, wage, group = id))
link_var(wage) <- "id"  # ON
link_type(wage) <- "self"
link_var(wage) <- NULL  # OFF


## example 4: Lynx - for posterity
# Good to show off wrapping to investigate irregular series
data(lynx)
qlynx <- qdata(data.frame(Time = 1:114, lynx))
print(qtime(Time, ~lynx, qlynx, shift = 1:12))


## example 5: Sunspots - for posterity
# Good to show off wrapping to investigate irregular series
data(sunspots)
qsun <- qdata(data.frame(Time = 1:2820, sunspots))
print(qtime(Time, ~sunspots, qsun, shift = c(1, (1:10) * 
    10)))


## example 6: Pigs
data(pigs)
qpig <- qdata(pigs)
print(qtime(TIME, ~GILTS + PROFIT + PRODUCTION + HERDSZ, 
    qpig, shift = c(1, 4)))

library(reshape)
pigGP <- pigs[, c(1, 7, 8)]
pigGP[, 2:3] <- rescaler(pigGP[, 2:3])
pigGP <- melt(pigGP, 1)
qpigGP <- qdata(pigGP)
print(qtime(TIME, ~value, qpigGP, group = variable, 
    shift = c(1, 4)))

link_var(qpigGP) <- "variable"  # ON
link_type(qpigGP) <- "self"
}
\seealso{
  Other plots: \code{\link{qbar}}, \code{\link{qboxplot}},
  \code{\link{qdensity}}, \code{\link{qhist}},
  \code{\link{qmap}}, \code{\link{qmosaic}},
  \code{\link{qmval}}, \code{\link{qparallel}},
  \code{\link{qspine}}
}
