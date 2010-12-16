## examples of qparallel()

## hints for interaction:
## drag with the right button to resize the brush; left button to move the brush

## options(verbose = TRUE)

library(cranvas)

data(nrcstat)
qnrc = qmutaframe(nrcstat)
rownames(qnrc) = paste(nrcstat$Institution.Name, nrcstat$Program.Name, sep = ' -> ')

## Overview: type, rankings
qparallel(qnrc, vars=10:13, main='Overview of Rankings', glyph='tick', center=median, horizontal=FALSE, boxplot=TRUE)
qnrc$.color = 'red'

data_selector(qnrc, "Institution.Name", "RGtk2")

## How to find out ISU by intersection and negation? public, midwest, large program

## show data labels
brush_attr(qnrc, '.label.show') = TRUE
brush_attr(qnrc, '.label.color') = 'yellow'

brush_attr(qnrc, '.label.show') = FALSE

qnrc$.color = 'red'

qparallel(qnrc, vars=14:19, main='Research, Student Support, Diversity', center=median, horizontal=FALSE, glyph='tick')

qparallel(qnrc, vars=20:68, main='Other Indicators', center=median, horizontal=FALSE, glyph='tick', lab.split=NULL, boxplot=TRUE, boxwex=.8)


## color palette
library(RColorBrewer)

## old iris...
##  create a mutaframe containing row attributes first
iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris = qmutaframe(iris, .brushed = FALSE, .color = iris.col)

qparallel(qiris)
qparallel(qiris, scale = "I")
qparallel(qiris, scale = "var")

# try other standardizing methods
st2 = function(x) ((x - min(x))/(max(x) - min(x)))^2
qparallel(qiris, scale = "st2")

## subsetting
qparallel(qiris, vars = c("Sepal.Length", "Sepal.Width", "Species"))
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
qparallel(qiris, glyph = 'tick')
qparallel(qiris, glyph = 'circle')
qparallel(qiris, glyph = 'square')
qparallel(qiris, glyph = 'triangle')

## order variables by MDS or ANOVA
qparallel(qiris, order = 'MDS')
qparallel(qiris, scale = 'I', order = 'MDS')
qparallel(qiris, order = 'ANOVA')
qparallel(qiris, scale = 'I', order = 'ANOVA')

## set color and print verbose timing
qiris$.color = rgb(1, 0, 0, 0.5)
qparallel(qiris, verbose = TRUE)

## the plot will be updated if we modify the mutaframe
qparallel(qiris)
for (i in 1:30) {
    qiris$Sepal.Length[1] = i
    qiris$.color[1] = sample(colors(), 1)
    Sys.sleep(.5)
}

## what if there are missing values?
xna = qmutaframe(sapply(iris, function(x) {
    x[sample(length(x), 50)] = NA
    x
}))
qparallel(xna)

## centering
qparallel(qiris, scale = 'I', center = median)
qparallel(qiris, scale = 'I', center = mean)
## to check we are really centered at the medians
qparallel(qiris, center = median, boxplot = TRUE)

## labeling
brush_attr(qiris, '.label.show') = TRUE
## we can also change the row names and the labels will change accordingly
rownames(qiris) = paste(abbreviate(iris$Species), 1:50, sep = '')

qmtcars = qmutaframe(mtcars)
qparallel(qmtcars)
qparallel(qmtcars, center = median)
qparallel(qmtcars, order = 'MDS')
qparallel(qmtcars, order = 'ANOVA')

## test speed
test.mat1 = qmutaframe(matrix(rnorm(1000 * 10), ncol = 10),
    .color = rgb(1, 0, 0, 0.2))
qparallel(test.mat1, mar = c(0.2, 0.1, 0.1, 0.1))

test.mat2 = qmutaframe(matrix(rnorm(1000 * 15), ncol = 15),
    .color = rgb(1, 0, 0, 0.2))
qparallel(test.mat2, boxplot = TRUE)

## slow for brushing in my laptop
test.mat3 = qmutaframe(matrix(rnorm(5000 * 10), ncol = 10),
     .color = rgb(1, 0, 0, 0.05))
qparallel(test.mat3, verbose = TRUE)

## speed tests
## on my laptop, 10000x10 takes 5 secs to build the brushing cache
## identifying is generally very fast once the cache was built
qhuge1 = qmutaframe(matrix(rbeta(1e+04 * 10, 5, 30), ncol = 10))
qparallel(qhuge1, verbose = TRUE)
## 30000x10 takes 45 seconds
qhuge2 = qmutaframe(matrix(rbeta(3e+04 * 10, 5, 30), ncol = 10))
qparallel(qhuge2, verbose = TRUE)
## 1 million points to torture Qt!!
qhuge3 = qmutaframe(matrix(rbeta(1e+05 * 10, 5, 30), ncol = 10))
qparallel(qhuge3, verbose = TRUE)

## linking two parcoords plots: split the data into 2 parts
testdata = qmutaframe(as.data.frame(matrix(rnorm(2000 * 10), ncol = 10)))
qparallel(testdata, vars = sprintf("V%d", 1:6))
qparallel(testdata, vars = sprintf("V%d", 4:10))

library(ggplot2)
qdiamonds = qmutaframe(diamonds, .color = rgb(1, 0, 0, .01))
qparallel(qdiamonds, vars = 1:7, glyph = 'line', jitter = ~ cut + color + clarity)
qdiamonds$.color = brewer.pal(5, "Set1")[as.integer(diamonds$cut)]
qparallel(qdiamonds, vars = 1:7, glyph = 'line', order = 'ANOVA')
qdiamonds$.color = brewer.pal(7, "Set1")[as.integer(diamonds$color)]
qparallel(qdiamonds, vars = 1:7, glyph = 'line', order = 'ANOVA')

## for large data, glyphs (short ticks) are automatically used instead of segments

## residential data: 18221x8
if (!require("YaleToolkit")) install.packages("YaleToolkit")
library(YaleToolkit)
data(NewHavenResidential)
qnhr = qmutaframe(NewHavenResidential, .color = rgb(1, 0, 0, 0.1))
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
qparallel(qnhr, jitter = c("bedrms", "zone"), amount = 0.2)


library(animation)
data(pollen)
qpollen = qmutaframe(pollen, .color = rgb(0, 0, 1, 0.01))
qparallel(qpollen)
