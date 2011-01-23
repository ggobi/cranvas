\name{qmutaframe}
\alias{qmutaframe}
\title{Create a Mutaframe from Data with Several Attributes for Interaction}
\usage{qmutaframe(data, ...)

}
\description{Create a mutaframe from data with several attributes for interaction}
\details{Create a mutaframe from data with several attributes for interaction:
first check if the names of some predefined row attributes (e.g. .color, .brushed)
exist in the data (will issue an error if this happens); then augment the ...
arguments to the data and convert the augmented data to a mutaframe; in the end
add some attributes to the mutaframe to control the appearance of elements for
interaction (e.g. the color of the brush, the size of the brushed objects, and
whether to show the labels of the brushed objects).}
\value{a mutaframe}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{data}{a data frame (typically); it will be coerced to a data
frame}
\item{...}{other attributes corresponding to rows such as colors,
sizes and so on}
}
\examples{iris0 = qmutaframe(iris, .color = "red", .brushed = FALSE)
## the line width >= 2 does not work for me, so use 1
brush_attr(iris0, ".brushed.size") = 1
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
brush_attr(iris0, ".brush.color") = "green"
## change brushed lines to black
brush_attr(iris0, ".brushed.color") = "black"}
















