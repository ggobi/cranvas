\name{qhist}
\alias{qhist}
\title{Create a hist plot..}
\usage{qhist(x, data, splitByCol = -1, horizontal = FALSE, 
    position = "none", color, fill, stroke, title, name, 
    ash = FALSE, start = min(data[[x]]), nbins = round(sqrt(nrow(data)), 
        0), binwidth, bin_algo_str, ...)

}


\description{Create a hist plot
Create a hist plot from 1-D numeric data}
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
print(qhist("Serve.Speed", qtennis, horizontal = FALSE))}







