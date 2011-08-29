library(cranvas)

data(nrcstat)
qnrc = qdata(nrcstat)
rownames(qnrc) = abbreviate(paste(nrcstat$Institution.Name, nrcstat$Program.Name, sep = " -> "))

## Overview: type, rankings
print(qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", glyph = "tick",
    horizontal = FALSE, boxplot = TRUE))

qnrc$.color = "red"

brush(qnrc, 'persistent') = TRUE  # begin persistent brushing
brush(qnrc, 'color') = 'brown'
## select other objects now
brush(qnrc, 'color') = 'green'
## again, select other objects
brush(qnrc, 'color') = 'yellow'
brush(qnrc, 'persistent') = FALSE  # transient brushing

record_selector("Institution.Name", qnrc)

## How to find out ISU by intersection and negation? public, midwest, large program

## show data labels (identifying)
brush(qnrc, "identify") = TRUE
brush(qnrc, "label.color") = "black"

brush(qnrc, "identify") = FALSE  # turn off identification

## brush one row, and the whole category gets brushed
link_var(qnrc) = 'Regional.Code'
link_type(qnrc) = 'self'


qparallel(vars = 14:19, data = qnrc, main = "Research, Student Support, Diversity",
    center = median, horizontal = FALSE, glyph = "tick")

qparallel(vars = 20:68, data = qnrc, main = "Other Indicators", center = median,
    horizontal = FALSE, glyph = "tick", boxplot = TRUE, boxwex = 0.8)

# Find ISU, and sort by best rank to least on ISU
median.centering <- function(x) {
    x <- (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
    x <- x - median(x, na.rm = T)
}
nrc.medctr <- apply(nrcstat[, 20:68], 2, median.centering)
var.ord <- order(nrc.medctr[13, ]) + 19
qparallel(vars = var.ord, data = qnrc, main = "Other Indicators", center = median,
    horizontal = FALSE, glyph = "tick", boxplot = TRUE, boxwex = 0.8)

## color palette
qnrc = qdata(nrcstat, color = Control)  # public or private
qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", boxplot = TRUE)


## old iris...
qiris = qdata(iris, color = Species)

qparallel(data = qiris, main = 'scale columns individually to [0, 1]')
qparallel(data = qiris, scale = "I", main = 'unscaled data')
qparallel(data = qiris, scale = "var", main = 'mean 0 variance 1')
qparallel(1:4, data = qiris, scale = "global", main = 'scale globally to [0, 1]')

# try other standardizing methods
st2 = function(x) ((x - min(x))/(max(x) - min(x)))^2
qparallel(data = qiris, scale = "st2")

## subsetting
qparallel(c("Sepal.Length", "Sepal.Width", "Species"), data = qiris)
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
qiris$.color = rgb(1, 0, 0, 0.5)

## the plot will be updated if we modify the mutaframe
qparallel(data = qiris)
for (i in 1:30) {
    qiris$Sepal.Length[1] = i
    qiris$.color[1] = sample(colors(), 1)
    Sys.sleep(0.5)
}

## what if there are missing values?
xna = qdata(sapply(iris, function(x) {
    x[sample(length(x), 50)] = NA
    x
}))
qparallel(data = xna)

## centering
qparallel(data = qiris, scale = "I", center = median)
qparallel(data = qiris, scale = "I", center = mean)
## to check we are really centered at the medians
qparallel(data = qiris, center = median, boxplot = TRUE)

## labeling
brush(qiris, "identify") = TRUE
## we can also change the row names and the labels will change accordingly
rownames(qiris) = paste(abbreviate(iris$Species), 1:50, sep = "")

qmtcars = qdata(mtcars)
qparallel(data = qmtcars)
qparallel(data = qmtcars, center = median)
qparallel(data = qmtcars, order = "MDS")
qparallel(data = qmtcars, order = "ANOVA")

## test speed
test.mat1 = qdata(matrix(rnorm(1000 * 10), ncol = 10), color = rgb(1,
    0, 0, 0.2))
qparallel(data = test.mat1)

test.mat2 = qdata(matrix(rnorm(1000 * 15), ncol = 15), color = rgb(1,
    0, 0, 0.2))
qparallel(data = test.mat2, boxplot = TRUE)

## slow for brushing in my laptop
test.mat3 = qdata(matrix(rnorm(5000 * 10), ncol = 10), color = rgb(1,
    0, 0, 0.05))

options(cranvas_debug = TRUE)
qparallel(data = test.mat3)

## speed tests
## on my laptop, 10000x10 takes 5 secs to build the brushing cache
## identifying is generally very fast once the cache was built
qhuge1 = qdata(matrix(rbeta(10000 * 10, 5, 30), ncol = 10))
qparallel(data = qhuge1)
## 30000x10 takes 45 seconds
qhuge2 = qdata(matrix(rbeta(30000 * 10, 5, 30), ncol = 10))
qparallel(data = qhuge2)
## 1 million points to torture Qt!!
qhuge3 = qdata(matrix(rbeta(1e+05 * 10, 5, 30), ncol = 10))
qparallel(data = qhuge3)

options(cranvas_debug = FALSE)


## for large data, glyphs (short ticks) are automatically used instead of
#   segments

## residential data: 18221x8
if (!require("YaleToolkit")) install.packages("YaleToolkit")
library(YaleToolkit)
data(NewHavenResidential)
qnhr = qdata(NewHavenResidential, color = rgb(1, 0, 0, 0.1))
qparallel(data = qnhr)

qparallel(1:4, data = qnhr)
qparallel(5:8, data = qnhr)


qnhr = qdata(NewHavenResidential, color = zone)
qparallel(data = qnhr)

qparallel(data = qnhr, horizontal = FALSE)

# jitter is hopeless for huge data...
qnhr$.color = rgb(1, 0, 0, 0.01)
qparallel(data = qnhr, jitter = "zone", amount = 0.3)
qparallel(data = qnhr, jitter = c("bedrms", "zone"), amount = 0.2)

if (require('animation')) {
  data(pollen, package = 'animation')
  qpollen = qdata(pollen, color = rgb(0, 0, 1, 0.01))
  qparallel(~.)
}

