\name{qdata}
\alias{qdata}
\title{Create a mutaframe from data with several attributes for interaction}
\usage{qdata(data, color = "black", fill = "grey30", 
    size = 1, brushed = FALSE, visible = TRUE)

}

\description{Create a mutaframe from data with several attributes for interaction.}
\details{First check if the names of some predefined row attributes
(e.g. .color, .brushed) exist in the data (will issue an error if
this happens); then augment the ...  arguments to the data and
convert the augmented data to a mutaframe; in the end add some
attributes to the mutaframe to control the appearance of elements
for interaction (e.g. the color of the brush, the size of the
brushed objects, and whether to show the labels of the brushed
objects).}
\value{a mutaframe with some attributes like the brush}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{data}{a data frame (typically); it will be coerced to a data
frame}
\item{color}{colors of rows (default black)}
\item{size}{sizes of rows (default 1)}
\item{brushed}{a logical vector indicating whether the rows are
brushed (default all \code{FALSE})}
\item{visible}{a logical vector indicating whether the rows are
visible (default all \code{TRUE})}
}
\examples{iris0 = qdata(iris, color = "red", brushed = FALSE)
## thicker line for brushed elements
brush(iris0, "size") = 3
qparallel(iris0)
## random colors
iris0$.color = sample(1:8, nrow(iris), replace = TRUE)
## change the colors to green
iris0$.color = "green"
## 'brushing' by command line
for (i in 1:10) {
    iris0$.brushed = sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)
    Sys.sleep(1)
}
## change the brush color to green
brush(iris0, "color") = "green"
## change brushed lines to black
brush(iris0, "color") = "black"}
















