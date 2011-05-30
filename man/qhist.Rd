\name{qhist}
\alias{qhist}
\title{Create a hist plot..}
\usage{qhist(x, data, splitByCol = -1, horizontal = FALSE, 
    position = "none", color, fill, stroke, title, name, 
    ash = FALSE, start = min(data[[x]]), nbins = round(sqrt(nrow(data)), 
        0), binwidth, bin_algo_str, xlim, ylim, ...)

}



\description{Create a hist plot
Create a hist plot from 1-D numeric data}
\details{keystroke Interactions:
right/left arrow shift anchors; 
up/down arrow increase/decrease bin size; 
M re-adjusts the maximum on the y-axis to 10\% above the bin count}
\author{Barret Schloerke, Di Cook, Heike Hofmann}
\keyword{hplot}
\arguments{\item{data}{mutaframe to use}
\item{x}{variable to plot}
\item{horizontal}{boolean to decide if the bars are horizontal or vertical}
\item{xlim}{= c(min, max) user specifed data range for the x axis, by default range(x)}
\item{ylim}{= c(min, max) user specifed data range for the y axis, by default range(y)}
\item{...}{arguments supplied to hist() or the hist layer}
}
\examples{## examples of qhist()
require(ggplot2)
require(cranvas)

data(tennis)
qtennis <- qdata(tennis)
print(qhist("First.Serve.Pct", qtennis, horizontal = FALSE))
print(qhist("Serve.Speed", qtennis, horizontal = FALSE))

print(qhist("First.Serve.Pct", qtennis, splitByCol = "Matches", 
    horizontal = FALSE))
# Position argument doesn't seem to work.
print(qhist("First.Serve.Pct", qtennis, splitByCol = "Matches", 
    horizontal = FALSE, postion = "dodge"))
print(qhist("First.Serve.Pct", qtennis, splitByCol = "Matches", 
    horizontal = FALSE, postion = "relative"))
print(qhist("First.Serve.Pct", qtennis, splitByCol = "Matches", 
    horizontal = FALSE, postion = "identity"))}













