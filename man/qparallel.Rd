\name{qparallel}
\alias{qparallel}
\title{Draw a parallel coordinates plot}
\usage{
qparallel(vars = ~., data, scale = "range", names = break_str(vars), na.action = na_impute, 
    center = NULL, order = c("none", "MDS", "ANOVA", "randomForest"), horizontal = FALSE, 
    glyph = c("auto", "line", "tick", "circle", "square", "triangle"), boxplot = FALSE, 
    width = NULL, jitter = NULL, amount = NULL, main = "", alpha = 1)
}
\arguments{
  \item{scale}{data standardizing method; possible values
  are \code{'range'} (scale columns individually to [0,
  1]), \code{'I'} (do not transform; use original values),
  \code{'var'} (make each column of mean 0 var 1), and
  \code{'global'} (scale all the columns to [0, 1] using
  global minimum and maximum); other character strings here
  means to use custom functions (see examples below)}

  \item{names}{the variable labels to use in the plot (by
  default, they are the variable names with
  non-alphanumeric characters replaced by line breaks
  \code{'\n'})}

  \item{na.action}{the function to deal with missing
  values}

  \item{center}{the function to calculate where to center
  all the variables (e.g. center at the medians), or a
  numeric value, or \code{NULL} (do not center)}

  \item{order}{methods to reorder the variables; see
  \code{\link{reorder_var}}}

  \item{horizontal}{logical: direction of axes (horizontal
  or vertical)}

  \item{glyph}{draw complete segments for all observations
  or other types of glyphs to represent observations (the
  latter has speed gain in case of large data)}

  \item{boxplot}{logical: overlay boxplots on top of the
  par-coords plot or not}

  \item{width}{width of boxplots}

  \item{jitter}{\code{NULL} (no jittering) or a character
  vector to jitter variables (usually those categorical
  variables)}

  \item{amount}{jitter amount}

  \item{main}{title of plot}

  \item{alpha}{value for alpha-blending}

  \item{vars}{variables to show in the plot: a character
  vector of variable names, or a numeric vector of column
  indices, or a two-sided formula like \code{~ x1 + x2 +
  x3} (without the left-hand side); see
  \code{\link{var_names}}}

  \item{data}{a mutaframe created by \code{\link{qdata}}}
}
\value{
  A par-coords plot
}
\description{
  This function creates a parallel coordinates plot
  (par-coords) for variables in a data, with each line
  representing a row.
}
\details{
  See \code{\link{common_key_press}} for a series of common
  interactions. Interactions specific to par-coords
  include: press \code{R} to toggle the min/max labels; the
  arrow keys are used to adjust the order of the variables
  and flip the values of variables (like a mirror
  reflection) when the axes are selected by the brush.
}
\examples{
library(cranvas)

### (1) flea data
data(flea, package = "tourr")
qflea <- qdata(flea, color = species)

## input variables
qparallel(~., data = qflea)  # all variables
qparallel(~aede1 + aede2 + aede3, data = qflea)  # 3 variables
qparallel(1:4, data = qflea)  # first 4 variables
qparallel(c("head", "aede1", "tars2", "aede3"), data = qflea)  # input variables by names

## scaling
qparallel(~., data = qflea, main = "scale columns individually to [0, 1]")
qparallel(~., data = qflea, scale = "I", main = "unscaled data")
qparallel(~., data = qflea, scale = "var", main = "mean 0 variance 1")
qparallel(~., data = qflea, scale = "global", main = "scale globally to [0, 1]")

## centering by median; add boxplots to assist understanding
qparallel(~., data = qflea, center = median, boxplot = TRUE)

## ordering
qparallel(~., data = qflea, order = "MDS")  # similar variables together
qparallel(~., data = qflea, order = "ANOVA")  # color is covariate, order by ANOVA p-value
## we can use arrow keys to manually order the variables

## horizontal direction
qparallel(~., data = qflea, horizontal = TRUE)

## use glyphs instead of segments
qparallel(~., data = qflea, glyph = "tick")
qparallel(~., data = qflea, glyph = "circle")

## jittering
qparallel(~., data = qflea, jitter = "species")

## categorical linking
id <- link_cat(qflea, "species")
## now brush on plots; will see all rows in the same category brushed

## stop categorical linking
remove_link(qflea, id)

### (2) NRC rankings
data(nrcstat)
qnrc <- qdata(nrcstat, color = RegCode)

## Overview: type, rankings
qparallel(13:10, main = "Overview of Rankings", glyph = "tick", horizontal = TRUE, 
    boxplot = TRUE)

## link to a droplist (institution names)
record_selector(Institution, qnrc)

## TODO: we need keyboard interactions here instead of command line
brush(qnrc, "persistent") <- TRUE  # begin persistent brushing
brush(qnrc, "color") <- "brown"
## select other objects now
brush(qnrc, "color") <- "green"
## again, select other objects
brush(qnrc, "color") <- "yellow"
brush(qnrc, "persistent") <- FALSE  # transient brushing

qparallel(vars = 14:19, data = qnrc, main = "Research, Student Support, Diversity", 
    center = median, horizontal = TRUE, glyph = "tick")


### (3) Missing values are imputed by 20\% below the mean
df <- as.data.frame(replicate(5, sample(c(rep(NA, 10), rnorm(100)))))
mf <- qdata(df)
qparallel(~., data = mf)

## see this by missing value plot
qmval(~., data = mf)



### (4) alpha transparency
if (require("animation")) {
    data(pollen, package = "animation")
    qpollen <- qdata(pollen)
    qparallel(~.)  # hold the minus key (-) till the plot is semi-transparent
}



### (5) pressure test
test.mat <- qdata(matrix(rnorm(3e+05 * 10), ncol = 10))
qparallel(~., data = test.mat)
## for large data, short ticks are automatically used instead of segments
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  Other plots: \code{\link{qbar}}, \code{\link{qboxplot}},
  \code{\link{qdensity}}, \code{\link{qhist}},
  \code{\link{qmval}}, \code{\link{qspine}},
  \code{\link{qtime}}
}
