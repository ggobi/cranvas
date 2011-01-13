\name{qhist}
\alias{qhist}
\title{Create a hist plot...}
\usage{qhist(data, xCol = 1, splitByCol = -1, horizontal = TRUE, position = "none", color, 
    fill, stroke, title, name = names(data), ash = FALSE, start = min(data[[xCol]]), nbins = 5, 
    binwidth, bin_algo_str, ...)

}
\description{Create a hist plot
Create a hist plot from 1-D numeric data}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{vector of numeric data to be made into a histogram}
\item{horizontal}{boolean to decide if the bars are horizontal or vertical}
\item{...}{arguments supplied to hist() or the hist layer}
}
\examples{# torture
rows <- 1e+06
bigData <- qmutaframe(data.frame(x = rnorm(rows), y = floor(rnorm(rows) * 7)))
qhist(bigData)

# each column is split evenly
qhist(bigData, splitByCol = "y", title = "Torture - stack")
qhist(bigData, splitByCol = "y", title = "Torture - stack", horizontal = FALSE)

# each column has similar height colors
qhist(bigData, splitByCol = "y", title = "Torture - dodge", position = "dodge")

# range from 0 to 1
qhist(bigData, splitByCol = "y", title = "Torture - relative", position = "relative")


# color tests
# all color is defined
qhist(mtcars, "disp", horizontal = TRUE, fill = "gold", stroke = "red4")

# stacked items
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "stack", 
    title = "mtcars - stack")

# raw value items
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "identity", 
    title = "mtcars - identity")

# dodged items
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "dodge", 
    title = "mtcars - dodge")

# range from 0 to 1
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "relative", 
    title = "mtcars - relative")}






























