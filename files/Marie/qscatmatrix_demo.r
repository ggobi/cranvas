#library(qtpaint)
#library(qtbase)
#library(plumbr)
#library(RColorBrewer)
#setwd("/home/marie/Documents/local_cranvas")
#source("/home/marie/Documents/cranvas/load.r")
source("qscatmatrix.r")

## old iris...
#  create mutaframes inside the data first
iris.col <- brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris <- qmutaframe(iris, .brushed = FALSE, .color = iris.col)
qiris <- qiris[,-5]
brush_attr(qiris, '.brushed.size') <- 2
brush_attr(qiris, '.brushed.color') <- "orange"
qiris[1:50,5] <- T 

qscatmatrix(data = qiris)


#to see hilighting
#qiris[1:25,5] <- F
