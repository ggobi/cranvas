\name{qdata}
\alias{qdata}
\title{Create a mutaframe from data with attributes for interaction}
\usage{qdata(data, color = "black", fill = "grey30", 
    size = 1, brushed = FALSE, visible = TRUE)

}

\description{Create a mutaframe from data with attributes for interaction.}
\details{This function will first check if the names of some predefined row
attributes (e.g. \code{.color}, \code{.brushed}) exist in the
column names of the data (will issue an error if they do); then
append these columns to the original data to create an augmented
data as a \code{\link[plumbr]{mutaframe}}; in the end add some
attributes to the mutaframe to control the behavior of interaction
(mainly the \code{\link{brush}} object and the
\code{\link{link}}ing specification).}
\value{a mutaframe with attributes for interaction}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link[plumbr]{mutaframe}}, \code{\link{brush}},
\code{\link{link}}}
\arguments{\item{data}{a data frame (typically); it will be coerced to a data
frame}
\item{color}{colors of graphical elements (default black)
corresponding to rows of data}
\item{fill}{colors for filling the graphical elements
(e.g. rectangles)}
\item{size}{sizes of rows (default 1)}
\item{brushed}{a logical vector indicating whether the rows are
brushed (default all \code{FALSE})}
\item{visible}{a logical vector indicating whether the rows are
visible (default all \code{TRUE})}
}
\examples{iris0 <- qdata(iris, color = "red", brushed = FALSE)
## thicker line for brushed elements
brush(iris0, "size") <- 3
qparallel(iris0)
## random colors
iris0$.color <- sample(1:8, nrow(iris), replace = TRUE)
## change the colors to green
iris0$.color <- "green"
## 'brushing' by command line
for (i in 1:10) {
    selected(iris0) <- sample(c(TRUE, FALSE), nrow(iris), 
        replace = TRUE)
    Sys.sleep(1)
}
## change the brush color to green
brush(iris0, "color") <- "green"
## change brushed lines to black
brush(iris0, "color") <- "black"}
















