\name{qscatter}
\alias{qscatter}
\title{Draw a scatterplot..}
\usage{qscatter(x, y, data, aspect.ratio, main, labeled = TRUE, 
    size = 4, alpha = 1, width = 600, height = 600, xlim, 
    ylim, xlab, ylab, cache = T, ...)

}

\description{Draw a scatterplot}
\details{arrow up/down: in-/de-crease size of points
arrow left/right: de-/in-crease alpha level (starts at alpha=1 by default)
Key 'z' toggle zoom on/off (default is off): mouse click & drag will specify a zoom window, reset to default window by click/no drag
Key 'x' toggle focal zoom on/off (default is off): mouse click & drag will specify a zoom window, zoom out by pressing shift key
Key 'r' resets data range to original scale}
\arguments{\item{x}{which designates variable displayed on the horizontal axis}
\item{y}{which designates variable displayed on the vertical axis}
\item{data}{mutaframe data to use}
\item{main}{main title for the plot}
\item{labeled}{whether axes should be labeled}
\item{size}{point size}
\item{alpha}{transparency level, 1=completely opaque}
\item{width}{width of plotting device (in pixels) at opening}
\item{height}{height of plotting device (in pixels) at opening}
\item{xlim}{= c(min, max) user specifed data range for the x axis, by default range(x)}
\item{ylim}{= c(min, max) user specifed data range for the y axis, by default range(y)}
\item{xlab}{label on horizontal axis, default is name of x variable}
\item{ylab}{label on vertical axis, default is name of y variable}
\item{cache}{boolean to turn cache on for layers, defaults to TRUE}
}
\examples{library(cranvas)
library(scales)

data(nrcstat)
nrcstat[, 26] <- -nrcstat[, 26]
colnames(nrcstat)[26] <- "Neg.Median.Time.to.Degree"

qnrc <- qdata(nrcstat)

print(qscatter(R.Rankings.5th.Percentile, R.Rankings.95th.Percentile, 
    qnrc))
print(qscatter(S.Rankings.5th.Percentile, S.Rankings.95th.Percentile, 
    qnrc))

data(pollen)
qpollen <- qdata(pollen)
qscatter(RIDGE, CRACK, qpollen)
# try zooming into the center

# categorical variable linking
data(flea)
qflea <- qdata(flea)
print(qscatter(tars1, aede1, qflea))

link_var(qflea) <- "species"
link_type(qflea) <- "self"

# Using species to color points
qflea <- qdata(flea, fill = dscale(flea$species, 
    hue_pal()), color = dscale(flea$species, hue_pal()))
print(qscatter(tars1, aede1, qflea))}



























