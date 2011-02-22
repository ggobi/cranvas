\name{qparallel}
\alias{qparallel}
\title{Parallel coordinates plot}
\usage{qparallel(data, vars, scale = "range", na.action = na.impute, 
    center, order = c("none", "MDS", "ANOVA"), horizontal = TRUE, 
    glyph = c("auto", "line", "tick", "circle", "square", 
        "triangle"), boxplot = FALSE, boxwex, jitter, amount, 
    mar = c(0.04, 0.04, 0.04, 0.04), main, lab.split = "[^[:alnum:]]")

}




\description{
Create a parallel coordinates plot from a data frame or matrix, with each
line representing a row.}
\value{NULL}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{data}{a mutaframe which is typically built upon a data frame
along with several row attributes}
\item{vars}{variables to show; can be a character vector (column
names), an integer vector (column indices) or a formula like '~ x1
+ x2'}
\item{scale}{standardizing method - 'range' --> [0, 1], 'I' --> do
nothing, 'var' --> mean 0 var 1, 'custom_function_name' --> use
your own function (see examples.R)}
\item{na.action}{the function to deal with missing values}
\item{center}{the function to calculate where to center all the
axes (e.g. center at the medians), or a numeric value, or
\code{NULL} (do not center)}
\item{order}{for \code{order = 'MDS'}, reorder the variables by
classical multidimensional scaling so that similar variables will
be arranged nearer to each other (categorical variables will be
put to the last); for \code{order = 'ANOVA'}, reorder the
variables by the p-values associated with the ANOVA based on each
variable versus the \code{data$.color} variable, so that the
variable with largest between-group difference will be put in the
first place, and so on; \code{order = 'none'} means keep the
original order.}
\item{horizontal}{logical: arrange variables in horizontal or
vertical direction}
\item{glyph}{draw complete segments for all observations or other
types of glyphs to represent observations (the latter can be more
efficient in case of large data)}
\item{boxplot}{logical: overlay boxplots on top of the par-coords
plot or not}
\item{boxwex}{width of boxplots}
\item{jitter}{NULL (no jittering) or a character vector to jitter
variables (usually those categorical vars)}
\item{amount}{jitter amount}
\item{mar}{margin (in proportion to the whole canvas)}
\item{main}{the title}
\item{lab.split}{the pattern to ``break'' the axis labels by
\code{'\n'}; the default \code{'[^[:alnum:]]'} means any
characters which are not alphanumeric will be replaced by
\code{'\n'}, i.e. the labels will be broken into several lines;
this can be useful when the axis labels are too long (if we do not
break them, they will be squeezed together along the axes); set
\code{lab.split = NULL} to keep the labels untouched}
}
\examples{## examples of qparallel()

## hints for interaction:
## drag with the right button to resize the brush; left
#   button to move the brush

## options(verbose = TRUE)

library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)

data(nrcstat)
qnrc = qmutaframe(nrcstat)
rownames(qnrc) = paste(nrcstat$Institution.Name, 
    nrcstat$Program.Name, sep = " -> ")

## Overview: type, rankings
qparallel(qnrc, vars = 13:10, main = "Overview of Rankings", 
    glyph = "tick", horizontal = FALSE, boxplot = TRUE)
qnrc$.color = "red"

record_selector(qnrc, vars = "Institution.Name")

## How to find out ISU by intersection and negation?
#   public, midwest, large program

## show data labels
brush_attr(qnrc, ".label.show") = TRUE
brush_attr(qnrc, ".label.color") = "yellow"

brush_attr(qnrc, ".label.show") = FALSE

qnrc$.color = "red"

qparallel(qnrc, vars = 14:19, main = "Research, Student Support, Diversity", 
    center = median, horizontal = FALSE, glyph = "tick")

qparallel(qnrc, vars = 20:68, main = "Other Indicators", 
    center = median, horizontal = FALSE, glyph = "tick", 
    lab.split = NULL, boxplot = TRUE, boxwex = 0.8)

# Find ISU, and sort by best rank to least on ISU
median.centering <- function(x) {
    x <- (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, 
        na.rm = T))
    x <- x - median(x, na.rm = T)
}
nrc.medctr <- apply(nrcstat[, 20:68], 2, median.centering)
var.ord <- order(nrc.medctr[13, ]) + 19
qparallel(qnrc, vars = var.ord, main = "Other Indicators", 
    center = median, horizontal = FALSE, glyph = "tick", 
    lab.split = NULL, boxplot = TRUE, boxwex = 0.8)

## color palette
library(RColorBrewer)

## old iris...
## create a mutaframe containing row attributes first
iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris = qmutaframe(iris, .brushed = FALSE, .color = iris.col)

qparallel(qiris)
qparallel(qiris, scale = "I")
qparallel(qiris, scale = "var")

# try other standardizing methods
st2 = function(x) ((x - min(x))/(max(x) - min(x)))^2
qparallel(qiris, scale = "st2")

## subsetting
qparallel(qiris, vars = c("Sepal.Length", "Sepal.Width", 
    "Species"))
## or formula interface
qparallel(qiris, vars = ~Sepal.Length + Sepal.Width)
# '.' means all variables in the data frame as usual
qparallel(qiris, vars = ~.)

## vertical
qparallel(qiris, horizontal = FALSE)

## jitter
qparallel(qiris, jitter = "Species", amount = 0.3)

## with boxplots
qparallel(qiris, boxplot = TRUE)
qparallel(qiris, scale = "I", boxplot = TRUE)
qparallel(qiris, boxplot = TRUE, horizontal = FALSE)

## with points rather than whole lines
qparallel(qiris, glyph = "tick")
qparallel(qiris, glyph = "circle")
qparallel(qiris, glyph = "square")
qparallel(qiris, glyph = "triangle")

## order variables by MDS or ANOVA
qparallel(qiris, order = "MDS")
qparallel(qiris, scale = "I", order = "MDS")
qparallel(qiris, order = "ANOVA")
qparallel(qiris, scale = "I", order = "ANOVA")

## set color and print verbose timing
qiris$.color = rgb(1, 0, 0, 0.5)
qparallel(qiris, verbose = TRUE)

## the plot will be updated if we modify the mutaframe
qparallel(qiris)
for (i in 1:30) {
    qiris$Sepal.Length[1] = i
    qiris$.color[1] = sample(colors(), 1)
    Sys.sleep(0.5)
}

## what if there are missing values?
xna = qmutaframe(sapply(iris, function(x) {
    x[sample(length(x), 50)] = NA
    x
}))
qparallel(xna)

## centering
qparallel(qiris, scale = "I", center = median)
qparallel(qiris, scale = "I", center = mean)
## to check we are really centered at the medians
qparallel(qiris, center = median, boxplot = TRUE)

## labeling
brush_attr(qiris, ".label.show") = TRUE
## we can also change the row names and the labels will
#   change accordingly
rownames(qiris) = paste(abbreviate(iris$Species), 
    1:50, sep = "")

qmtcars = qmutaframe(mtcars)
qparallel(qmtcars)
qparallel(qmtcars, center = median)
qparallel(qmtcars, order = "MDS")
qparallel(qmtcars, order = "ANOVA")

## test speed
test.mat1 = qmutaframe(matrix(rnorm(1000 * 10), 
    ncol = 10), .color = rgb(1, 0, 0, 0.2))
qparallel(test.mat1, mar = c(0.2, 0.1, 0.1, 0.1))

test.mat2 = qmutaframe(matrix(rnorm(1000 * 15), 
    ncol = 15), .color = rgb(1, 0, 0, 0.2))
qparallel(test.mat2, boxplot = TRUE)

## slow for brushing in my laptop
test.mat3 = qmutaframe(matrix(rnorm(5000 * 10), 
    ncol = 10), .color = rgb(1, 0, 0, 0.05))
qparallel(test.mat3, verbose = TRUE)

## speed tests
## on my laptop, 10000x10 takes 5 secs to build the
#   brushing cache
## identifying is generally very fast once the cache
#   was built
qhuge1 = qmutaframe(matrix(rbeta(10000 * 10, 5, 
    30), ncol = 10))
qparallel(qhuge1, verbose = TRUE)
## 30000x10 takes 45 seconds
qhuge2 = qmutaframe(matrix(rbeta(30000 * 10, 5, 
    30), ncol = 10))
qparallel(qhuge2, verbose = TRUE)
## 1 million points to torture Qt!!
qhuge3 = qmutaframe(matrix(rbeta(1e+05 * 10, 5, 
    30), ncol = 10))
qparallel(qhuge3, verbose = TRUE)

## linking two parcoords plots: split the data into 2
#   parts
testdata = qmutaframe(as.data.frame(matrix(rnorm(2000 * 
    10), ncol = 10)))
qparallel(testdata, vars = sprintf("V\%d", 1:6))
qparallel(testdata, vars = sprintf("V\%d", 4:10))

library(ggplot2)
qdiamonds = qmutaframe(diamonds, .color = rgb(1, 
    0, 0, 0.01))
qparallel(qdiamonds, vars = 1:7, glyph = "line", 
    jitter = ~cut + color + clarity)
qdiamonds$.color = brewer.pal(5, "Set1")[as.integer(diamonds$cut)]
qparallel(qdiamonds, vars = 1:7, glyph = "line", 
    order = "ANOVA")
qdiamonds$.color = brewer.pal(7, "Set1")[as.integer(diamonds$color)]
qparallel(qdiamonds, vars = 1:7, glyph = "line", 
    order = "ANOVA")

## for large data, glyphs (short ticks) are
#   automatically used instead of segments

## residential data: 18221x8
if (!require("YaleToolkit")) install.packages("YaleToolkit")
library(YaleToolkit)
data(NewHavenResidential)
qnhr = qmutaframe(NewHavenResidential, .color = rgb(1, 
    0, 0, 0.1))
qparallel(qnhr)

qparallel(qnhr, vars = names(NewHavenResidential)[1:4])
qparallel(qnhr, vars = names(NewHavenResidential)[5:8])


# ggplot2
library(ggplot2)
ggpcp(NewHavenResidential) + geom_line()

# lattice
library(lattice)
parallel(NewHavenResidential)

qnhr$.color = brewer.pal(3, "Set1")[as.integer(NewHavenResidential$zone)]
qparallel(qnhr)

qparallel(qnhr, horizontal = FALSE)

# jitter is hopeless for huge data...
qnhr$.color = rgb(1, 0, 0, 0.01)
qparallel(qnhr, jitter = "zone", amount = 0.3)
qparallel(qnhr, jitter = c("bedrms", "zone"), 
    amount = 0.2)


library(animation)
data(pollen)
qpollen = qmutaframe(pollen, .color = rgb(0, 0, 
    1, 0.01))
qparallel(qpollen)}









































































































































































































