\name{qdata}
\alias{qdata}
\title{Create a mutaframe from data with attributes for interaction}
\usage{
  qdata(data, color = "black", fill = "grey30", size = 1,
  brushed = FALSE, visible = TRUE)
}
\arguments{
  \item{data}{a data frame (it will be coerced to a data
  frame if it is not)}

  \item{color}{colors of graphical elements (default black)
  corresponding to rows of data; it can be a vector of
  valid R colors, or a name of variable in \code{data}
  (must be either a factor or a numeric variable), or an R
  expression to calculate colors}

  \item{fill}{colors for filling the graphical elements
  (e.g. rectangles); possible values are similar to
  \code{color}}

  \item{size}{sizes of rows (default 1); possible values
  are similar to \code{color}, but when using a variable to
  generate sizes, it must be a numeric variable}

  \item{brushed}{a logical vector indicating whether the
  rows are brushed (default all \code{FALSE})}

  \item{visible}{a logical vector indicating whether the
  rows are visible (default all \code{TRUE})}
}
\value{
  a mutaframe with attributes for interaction
}
\description{
  This function will first check if the names of some
  pre-defined row attributes (e.g. \code{.color},
  \code{.brushed}) exist in the column names of the data
  (will issue an error if they do); then append these
  columns to the original data to create an augmented data
  as a \code{\link[plumbr]{mutaframe}}; in the end add some
  attributes to the mutaframe for the purpose of
  interaction (mainly the \code{\link{brush}} object and
  the \code{\link{link}}ing specification). A shadow matrix
  will be attached if any missing values are present in the
  original data.
}
\details{
  When the three arguments \code{color}, \code{fill} and
  \code{size} take values as variable names in \code{data},
  default palettes will be used to generate colors and
  sizes. The sequential color gradient palette
  (\code{\link[scales]{seq_gradient_pal}}) will be applied
  to continuous variables, and the hue palette
  (\code{\link[scales]{hue_pal}}) will be applied to
  categorical variables. The area palette
  (\code{\link[scales]{area_pal}}) is used to create a size
  vector when the size variable is continuous. If any
  palette is used, an attribute \code{attr(data, 'Scales')}
  will be attached to the returned mutaframe, which will
  help specific plots to generate legends.
}
\examples{
library(cranvas)
data(tennis)
str(tennis)
qtennis <- qdata(tennis, color = Aces, fill = Aces)  # more Aces, closer to red; less, blue

qhist2(Aces, data = qtennis, main = "Number of Aces")
attr(qtennis, "Scales")  # the scales information

selected(qtennis)[1:10] <- TRUE  # brush the first 10 cases

b <- brush(qtennis)  # the brush object
b$style  # style the brush rectangle
b$style$color <- "brown"  # a brown brush

b$color  # color of brushed elements

b$color <- "cyan"  # brushed elements become cyan

## linking by categorical variable Country
link_var(qtennis) <- "Country"
link_type(qtennis) <- "self"
## now all elements in the same category as those brushed
#   will be brushed

attr(qtennis, "Shadow")  # should be NULL, since no misssing values here


## we can also use the default black color and gray fill
qtennis <- qdata(tennis)
qhist2(Double.Faults, data = qtennis)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link[plumbr]{mutaframe}}, \code{\link{brush}},
  \code{\link{link}}
}
