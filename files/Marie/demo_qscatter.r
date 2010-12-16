source("qscatter.r")
require(RColorBrewer)
require(plumbr)

iris.col <- brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris <- qmutaframe(iris, .brushed = FALSE, .color = iris.col)

brush_attr(qiris, '.brushed.size') <- 2
brush_attr(qiris, '.brushed.color') <- "orange"

#### demo 1: qscatter on it's own
display <- qscatter(data = qiris, form = Petal.Length~Sepal.Width)
print(display)

####### demo 2: add mosaic plots
source("../Heike/mosaic-hilite.r", chdir = T)
print(qmosaic(qiris, ~Species,"hbar"))

####### demo 3: a scatterplot with no axes, labels, etc
#display <- qscatter(data = qiris, form = Petal.Length~Sepal.Width, labeled = F)

#source("../Heike/mosaic-hilite.r", chdir = T)
#print(qmosaic(qiris, ~Species,"hbar"))

