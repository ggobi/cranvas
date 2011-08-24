\name{qparallel}
\alias{qparallel}
\title{Parallel coordinates plot}
\usage{
qparallel(vars, data = last_data(), scale = "range", 
    names = break_str(vars), na.action = na_impute, center = NULL, 
    order = c("none", "MDS", "ANOVA", "randomForest"), horizontal = TRUE, 
    glyph = c("auto", "line", "tick", "circle", "square", "triangle"), 
    boxplot = FALSE, boxwex, jitter = NULL, amount = NULL, main, 
    alpha = 1, draw.range = TRUE)
}
\arguments{
  \item{vars}{variables to show; can be a character vector
  (column names), an integer vector (column indices) or a
  formula like '~ x1 + x2'; if missing or it is a formula
  that contains a dot (e.g. \code{ ~ .}), all variables in
  the data except those whose names start with a dot will
  be used}

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

  \item{horizontal}{logical: arrange variables in
  horizontal or vertical direction}

  \item{glyph}{draw complete segments for all observations
  or other types of glyphs to represent observations (the
  latter can be more efficient in case of large data)}

  \item{boxplot}{logical: overlay boxplots on top of the
  par-coords plot or not}

  \item{boxwex}{width of boxplots}

  \item{jitter}{NULL (no jittering) or a character vector
  to jitter variables (usually those categorical vars)}

  \item{amount}{jitter amount}

  \item{main}{the title}

  \item{alpha}{the opacity value}

  \item{draw.range}{whether to draw the range values (min
  and max for each variable)}

  \item{data}{a mutaframe created by \code{\link{qdata}}}
}
\value{
  a plot object with attributes
}
\description{
  Create a parallel coordinates plot from a data frame or
  matrix, with each line representing a row.
}
\details{
  We can turn on/off identify by setting \code{brush(data,
  'identify')} to be \code{TRUE}/\code{FALSE}. In the
  identify mode, the rows of data in a small neighborhood
  of the mouse will be shown on the screen. In the brush
  mode, we can use the left button of the mouse to drag a
  brush over the plot, and the brushed elements will be
  highlighted; the right button is used to resize the
  brush. A simple click will not activate the brush -- the
  brush works only when the mouse is moved to a difference
  location with one button being pressed.

  Several key stroke interactions are available as well: in
  the brush mode, we can hold the key \code{A} for
  \code{AND} operations, i.e. only the elements which were
  brushed in the last time \emph{and} are also brushed this
  time will be finally brushed. Similarly, \code{O} is for
  \code{OR}; \code{X} for \code{XOR}; \code{N} for
  \code{NOT} and \code{C} for \code{COMPLEMENT}. We can
  press \code{R} to toggle the min/max labels. Keys
  \code{+} and \code{-} can adjust the opacity of the plot
  linearly. \code{Delete} can make the brushed elements
  invisible while \code{F5} will make all the elements
  visible. The arrow keys are used to adjust the order of
  the variables and flip the values of variables (like a
  mirror reflection). \code{PageUp} and \code{PageDown} can
  be used to go back and forth in the brush history.
}
\examples{
## interaction: drag with the right button to resize the
#   brush; left button to move it

## options(cranvas_debug = TRUE)

library(cranvas)

data(nrcstat)
qnrc <- qdata(nrcstat)
rownames(qnrc) <- abbreviate(paste(nrcstat$Institution.Name, 
    nrcstat$Program.Name, sep = " -> "))

## Overview: type, rankings
print(qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", 
    glyph = "tick", horizontal = FALSE, boxplot = TRUE))

qnrc$.color <- "red"

brush(qnrc, "persistent") <- TRUE  # begin persistent brushing
brush(qnrc, "color") <- "brown"
## select other objects now
brush(qnrc, "color") <- "green"
## again, select other objects
brush(qnrc, "color") <- "yellow"
brush(qnrc, "persistent") <- FALSE  # transient brushing

record_selector("Institution.Name", qnrc)

## How to find out ISU by intersection and negation?
#   public, midwest, large program

## show data labels (identifying)
brush(qnrc, "identify") <- TRUE
brush(qnrc, "label.color") <- "black"

brush(qnrc, "identify") <- FALSE  # turn off identification

## brush one row, and the whole category gets brushed
link_var(qnrc) <- "Regional.Code"
link_type(qnrc) <- "self"


qparallel(vars = 14:19, data = qnrc, main = "Research, Student Support, Diversity", 
    center = median, horizontal = FALSE, glyph = "tick")

qparallel(vars = 20:68, data = qnrc, main = "Other Indicators", 
    center = median, horizontal = FALSE, glyph = "tick", boxplot = TRUE, 
    boxwex = 0.8)

# Find ISU, and sort by best rank to least on ISU
median.centering <- function(x) {
    x <- (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, 
        na.rm = T))
    x <- x - median(x, na.rm = T)
}
nrc.medctr <- apply(nrcstat[, 20:68], 2, median.centering)
var.ord <- order(nrc.medctr[13, ]) + 19
qparallel(vars = var.ord, data = qnrc, main = "Other Indicators", 
    center = median, horizontal = FALSE, glyph = "tick", boxplot = TRUE, 
    boxwex = 0.8)

## color palette
qnrc <- qdata(nrcstat, color = Control)  # public or private
qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", 
    boxplot = TRUE)


## old iris...
qiris <- qdata(iris, color = Species)

qparallel(data = qiris, main = "scale columns individually to [0, 1]")
qparallel(data = qiris, scale = "I", main = "unscaled data")
qparallel(data = qiris, scale = "var", main = "mean 0 variance 1")
qparallel(1:4, data = qiris, scale = "global", main = "scale globally to [0, 1]")

# try other standardizing methods
st2 <- function(x) ((x - min(x))/(max(x) - min(x)))^2
qparallel(data = qiris, scale = "st2")

## subsetting
qparallel(c("Sepal.Length", "Sepal.Width", "Species"), 
    data = qiris)
## or formula interface
qparallel(~Sepal.Length + Sepal.Width, data = qiris)
# '.' means all variables in the data frame as usual
qparallel(~., data = qiris)

## vertical
qparallel(data = qiris, horizontal = FALSE)

## jitter
qparallel(data = qiris, jitter = "Species", amount = 0.3)

## with boxplots
qparallel(data = qiris, boxplot = TRUE)
qparallel(data = qiris, scale = "I", boxplot = TRUE)
qparallel(data = qiris, boxplot = TRUE, horizontal = FALSE)

## with points rather than whole lines
qparallel(data = qiris, glyph = "tick")
qparallel(data = qiris, glyph = "circle")
qparallel(data = qiris, glyph = "square")
qparallel(data = qiris, glyph = "triangle")

## order variables by MDS or ANOVA
qparallel(data = qiris, order = "MDS")
qparallel(data = qiris, scale = "I", order = "MDS")
qparallel(data = qiris, order = "ANOVA")
qparallel(data = qiris, scale = "I", order = "ANOVA")

## set color
qiris$.color <- rgb(1, 0, 0, 0.5)

## the plot will be updated if we modify the mutaframe
qparallel(data = qiris)
for (i in 1:30) {
    qiris$Sepal.Length[1] <- i
    qiris$.color[1] <- sample(colors(), 1)
    Sys.sleep(0.5)
}

## what if there are missing values?
xna <- qdata(sapply(iris, function(x) {
    x[sample(length(x), 50)] <- NA
    x
}))
qparallel(data = xna)

## centering
qparallel(data = qiris, scale = "I", center = median)
qparallel(data = qiris, scale = "I", center = mean)
## to check we are really centered at the medians
qparallel(data = qiris, center = median, boxplot = TRUE)

## labeling
brush(qiris, "identify") <- TRUE
## we can also change the row names and the labels will
#   change accordingly
rownames(qiris) <- paste(abbreviate(iris$Species), 
    1:50, sep = "")

qmtcars <- qdata(mtcars)
qparallel(data = qmtcars)
qparallel(data = qmtcars, center = median)
qparallel(data = qmtcars, order = "MDS")
qparallel(data = qmtcars, order = "ANOVA")

## test speed
test.mat1 <- qdata(matrix(rnorm(1000 * 10), ncol = 10), 
    color = rgb(1, 0, 0, 0.2))
qparallel(data = test.mat1)

test.mat2 <- qdata(matrix(rnorm(1000 * 15), ncol = 15), 
    color = rgb(1, 0, 0, 0.2))
qparallel(data = test.mat2, boxplot = TRUE)

## slow for brushing in my laptop
test.mat3 <- qdata(matrix(rnorm(5000 * 10), ncol = 10), 
    color = rgb(1, 0, 0, 0.05))

options(cranvas_debug = TRUE)
qparallel(data = test.mat3)

## speed tests
## on my laptop, 10000x10 takes 5 secs to build the
#   brushing cache
## identifying is generally very fast once the cache was
#   built
qhuge1 <- qdata(matrix(rbeta(10000 * 10, 5, 30), ncol = 10))
qparallel(data = qhuge1)
## 30000x10 takes 45 seconds
qhuge2 <- qdata(matrix(rbeta(30000 * 10, 5, 30), ncol = 10))
qparallel(data = qhuge2)
## 1 million points to torture Qt!!
qhuge3 <- qdata(matrix(rbeta(1e+05 * 10, 5, 30), ncol = 10))
qparallel(data = qhuge3)

options(cranvas_debug = FALSE)


## for large data, glyphs (short ticks) are automatically
#   used instead of
#   segments

## residential data: 18221x8
if (!require("YaleToolkit")) install.packages("YaleToolkit")
library(YaleToolkit)
data(NewHavenResidential)
qnhr <- qdata(NewHavenResidential, color = rgb(1, 
    0, 0, 0.1))
qparallel(data = qnhr)

qparallel(1:4, data = qnhr)
qparallel(5:8, data = qnhr)


qnhr <- qdata(NewHavenResidential, color = zone)
qparallel(data = qnhr)

qparallel(data = qnhr, horizontal = FALSE)

# jitter is hopeless for huge data...
qnhr$.color <- rgb(1, 0, 0, 0.01)
qparallel(data = qnhr, jitter = "zone", amount = 0.3)
qparallel(data = qnhr, jitter = c("bedrms", "zone"), 
    amount = 0.2)


library(animation)
data(pollen)
qpollen <- qdata(pollen, color = rgb(0, 0, 1, 0.01))
qparallel(data = qpollen)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
