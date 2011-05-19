\name{qscatter}
\alias{qscatter}
\title{Draw a scatterplot..}
\usage{qscatter(data, x, y, aspect.ratio, main, labeled = TRUE, 
    size = 2, alpha = 1, datalims, cache = T, ...)

}

\description{Draw a scatterplot}
\arguments{\item{data}{mutaframe data to use}
\item{x}{which designates variable displayed on the horizontal axis}
\item{y}{which designates variable displayed on the vertical axis}
\item{main}{main title for the plot}
\item{labeled}{whether axes should be labeled}
\item{size}{point size}
\item{alpha}{transparency level, 1=completely opaque}
\item{datalims}{user specifed data ranges xmin, xmax, ymin, ymax}
\item{cache}{boolean to turn cache on for layers, defaults to TRUE}
}
\examples{require(cranvas)

data(nrcstat)
nrcstat[, 26] <- -nrcstat[, 26]
colnames(nrcstat)[26] <- "Neg.Median.Time.to.Degree"

qnrc = qdata(nrcstat)

print(qscatter(qnrc, R.Rankings.5th.Percentile, 
    R.Rankings.95th.Percentile))
print(qscatter(qnrc, S.Rankings.5th.Percentile, 
    S.Rankings.95th.Percentile))}









