\name{qscatter}
\alias{qscatter}
\title{Draw a scatterplot..}
\usage{qscatter(data, x, y, aspect.ratio, main, labeled = TRUE, 
    size = 2, alpha = 1, ...)

}
\description{Draw a scatterplot}
\arguments{\item{data}{mutaframe data to use}
\item{x}{which designates variable displayed on the horizontal axis}
\item{y}{which designates variable displayed on the vertical axis}
\item{main}{main title for the plot}
\item{labeled}{whether axes should be labeled}
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









