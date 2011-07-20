\name{qdensity}
\alias{qdensity}
\title{Draw a univariate density plot, with a rug plot underneath..}
\usage{qdensity(x, data, main, binwidth, size = 4, alpha = 0.5, 
    xlim, ylim, xlab, ylab, cache = T, ...)

}

\description{Draw a univariate density plot, with a rug plot underneath}
\details{arrow up/down: in-/de-crease size of points

arrow -/+: de-/in-crease alpha level (starts at alpha=1 by default)

arrow left/right: de-/in-crease binwidth for density

Key 'z' toggle zoom on/off (default is off): mouse click & drag will specify a zoom window, reset to default window by click/no drag

Key 'x' toggle focal zoom on/off (default is off): mouse click & drag will specify a zoom window, zoom out by pressing shift key

Key 'r' resets data range to original scale}
\arguments{\item{x}{which designates variable displayed on the horizontal axis}
\item{data}{mutaframe data to use}
\item{size}{point size}
\item{alpha}{transparency level, 1=completely opaque}
\item{xlim}{= c(min, max) user specifed data range for the x axis, by default range(x)}
\item{ylim}{= c(min, max) user specifed data range for the y axis, by default range(y)}
\item{xlab}{label on horizontal axis, default is name of x variable}
\item{ylab}{label on vertical axis, default is name of y variable}
\item{cache}{boolean to turn cache on for layers, defaults to TRUE}
}
\examples{# Examples for qdensity
library(cranvas)

data(tennis)
qtennis <- qdata(tennis)

print(qdensity(First.Serve.Pct, qtennis))
print(qdensity(Second.Serve.Pts, qtennis))
print(qdensity(Serve.Speed, qtennis))

record_selector("Name", qtennis)

data(pollen)
qpollen <- qdata(pollen)
print(qdensity(RIDGE, qpollen))

data(flea)
qflea <- qdata(flea)

print(qdensity(tars1, qflea))
print(qdensity(tars2, qflea))
print(qdensity(aede1, qflea))
print(qdensity(aede3, qflea))}






















