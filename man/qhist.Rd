\name{qhist}
\alias{qhist}
\title{Create a hist plot..}
\usage{qhist(x, data, splitByCol = -1, horizontal = FALSE, 
    position = "none", color, fill, stroke, title, name, 
    ash = FALSE, start = min(data[[x]]), nbins = round(sqrt(nrow(data)), 
        0), binwidth, bin_algo_str, xlims, ylims, ...)

}



\description{Create a hist plot
Create a hist plot from 1-D numeric data}
\details{Interactions:
right/left arrow shift anchors
up/down arrow increase/decrease bin size}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{mutaframe to use}
\item{x}{variable to plot}
\item{horizontal}{boolean to decide if the bars are horizontal or vertical}
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













