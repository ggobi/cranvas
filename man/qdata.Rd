\name{qdata}
\alias{qdata}
\title{Create a mutaframe from data with attributes for interaction}
\usage{
qdata(data, color = "gray15", border = color, size = 1, 
    brushed = FALSE, visible = TRUE)
}
\arguments{
  \item{data}{a data frame (it will be coerced to a data
  frame if it is not)}

  \item{color}{colors of graphical elements (default dark
  gray) corresponding to rows of data; it can be a vector
  of valid R colors, or a name of variable in \code{data}
  (must be either a factor or a numeric variable), or an R
  expression to calculate colors; \code{color} is used to
  fill the interior of graphical elements}

  \item{border}{colors for the border of graphical elements
  (e.g. rectangles); \code{NA} means to suppress the
  border}

  \item{size}{sizes of rows (default 1); possible values
  are similar to \code{color}, but when using a variable to
  generate sizes, it must be a numeric variable}

  \item{brushed}{a logical vector indicating which rows are
  brushed (default all \code{FALSE})}

  \item{visible}{a logical vector indicating which rows are
  visible (default all \code{TRUE})}
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
  When the three arguments \code{color}, \code{border} and
  \code{size} take values as variable names in \code{data},
  default palettes will be used to generate colors and
  sizes. The sequential color gradient palette
  (\code{\link[scales]{seq_gradient_pal}}) will be applied
  to continuous variables, and the hue palette
  (\code{\link[scales]{hue_pal}}) will be applied to
  categorical variables. The area palette
  (\code{\link[scales]{area_pal}}) is used to create a size
  vector when the size variable is continuous. An attribute
  \code{attr(data, 'Scales')} is attached to the returned
  mutaframe, which will help specific plots to generate
  legends. This attribute is of the form \code{list(color =
  list(label, variable, palette))}. Whenever any component
  is changed, the corresponding aesthetics will be updated
  automatically; for example, if we change the palette
  function for \code{color}, the colors \code{data$.color}
  will be updated using the new palette. See
  \code{\link{color_pal<-}} for a list of functions on how
  to modify scales information.
}
\examples{
library(cranvas)
data(tennis)
str(tennis)
## more Aces, closer to red; less, blue; higher speed,
#   larger points
qtennis <- qdata(tennis, color = Aces, size = Serve.Speed)

qhist(Aces, data = qtennis, main = "Number of Aces")
ls.str(attr(qtennis, "Scales"))  # the scales information

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


## we can also use the default dark gray
qtennis <- qdata(tennis)
qhist(Double.Faults, data = qtennis)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link[plumbr]{mutaframe}}, \code{\link{brush}},
  \code{\link{link}}
}
