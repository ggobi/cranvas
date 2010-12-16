setwd("/Users/dicook/cranvas")
#load(".Rdata")

library(qtpaint)
library(plumbr)
library(RColorBrewer)

# options(verbose = TRUE)
# Yihui
source("/Users/dicook/cranvas/code/files/utilities/optimization.R")
source("/Users/dicook/cranvas/code/files/utilities/interaction.R")
source("/Users/dicook/cranvas/code/files/utilities/data.R")
source("/Users/dicook/cranvas/code/files/Yihui/qparallel.R")

## old iris...
#  create mutaframes inside the data first
iris.col = brewer.pal(3, "Set1")[as.integer(iris$Species)]
qiris = qmutaframe(iris, .brushed = FALSE, .color = iris.col)

brush_attr(qiris, '.brushed.size') <- 1

qparallel(qiris)
qparallel(qiris)

# Hadley's tour
source("/Users/dicook/cranvas/code/files/Hadley/tourr-gui.r")
# gui_xy(olive)

# Di's modifications
library(qtbase)
library(qtpaint)
library(ggplot2, warn.conflicts = FALSE)
library(tourr, warn.conflicts = FALSE)
olive$region <- factor(olive$region)
library(colorspace)
library(RGtk2)
library(gWidgets)
library(plumbr)
library(RColorBrewer)
source("/Users/dicook/cranvas/code/files/utilities/interaction.R")
source("/Users/dicook/cranvas/code/files/Di/tourr-gui.r")

olive.col <- brewer.pal(3, "Set1")[as.integer(olive$region)]
qolive <- qmutaframe(olive, .brushed = FALSE, .color = olive.col)
qparallel(qolive)
gui_xy(qolive)

qolive$.brushed[1:300]<-TRUE

# Mosaic plots
require(productplots)
source("/Users/dicook/cranvas/code/files/Heike/mosaic-hilite.r")
source("/Users/dicook/cranvas/code/files/Heike/labels.r")
source("/Users/dicook/cranvas/code/files//utilities/api-sketch.r")
source("/Users/dicook/cranvas/code/files//utilities/helper.r")
source("/Users/dicook/cranvas/code/files//utilities/axes.r")
source("/Users/dicook/cranvas/code/files//utilities/interaction.R")

qmosaic(qiris, ~Species,"hbar")
qparallel(qiris)

qhappy <- qmutaframe(happy, .brushed = FALSE)
brush_attr(qhappy, '.brushed.color') <- "yellow"
qmosaic(qhappy, ~ happy, c("hbar"))
qmosaic(qhappy, ~ degree+sex+happy, c("vspine","hspine","hspine"))

# Tengfei's code
source('/Users/dicook/cranvas/code/files/Tengfei/eos/R/qcircle-utils.R')
source('/Users/dicook/cranvas/code/files/Tengfei/eos/R/qcircle-painter.R')
options(stringsAsFactors=FALSE)

# NRC data
nrcstat = read.csv('/Users/dicook/cranvas/code/files/Yihui/nrcstat.csv')
qnrc = qmutaframe(nrcstat)
rownames(qnrc)=paste(nrcstat$Institution.Name, nrcstat$Program.Name, sep = ' -> ')
nms = names(nrcstat)

## Overview: type, rankings
brush_attr(qnrc, '.label.show') <- TRUE
brush_attr(qnrc, '.label.color') <- 'black'

#brush_attr(qnrc, '.label.show') <- FALSE

qnrc$.color = 'red'

qparallel(qnrc, vars = nms[10:13], main = 'Overview of Rankings', horizontal=FALSE, glyph="circle")
qparallel(qnrc, vars = nms[20:68], main = 'Criteria', horizontal=FALSE, center=median, glyph="circle")

qparallel(qnrc, vars = nms[10:13], main = 'Overview of Rankings', horizontal=FALSE)
qparallel(qnrc, vars = nms[20:68], main = 'Criteria', horizontal=FALSE, center=median, scale="I")
qparallel(qnrc, vars = nms[20:68], main = 'Criteria', horizontal=FALSE, scale="I")
qparallel(qnrc, vars = nms[20:68], main = 'Criteria', horizontal=FALSE, boxplot=TRUE, glyph="circle")

# Barret's code
source("/Users/dicook/cranvas/code/R/_api-sketch.r")
source("/Users/dicook/cranvas/code/R/_axes.r")
source("/Users/dicook/cranvas/code/R/_data.r")
source("/Users/dicook/cranvas/code/R/_helper.r")
source("/Users/dicook/cranvas/code/R/_interaction.R")
source("/Users/dicook/cranvas/code/R/_optimization.R")
source("/Users/dicook/cranvas/code/R/bar.r")
source("/Users/dicook/cranvas/code/R/bin.r")
source("/Users/dicook/cranvas/code/R/bprint.r")

rows <- 1000000
bigData <- qmutaframe(data.frame(x = rnorm(rows), y = floor(rnorm(rows) * 7)))
qhist(bigData)

# each column is split evenly
qhist(bigData, splitByCol = "y", title = "Toture - stack")
qhist(bigData, splitByCol = "y", title = "Toture - stack", horizontal = FALSE)

# each column has similar height colors
qhist(bigData, splitByCol = "y", title = "Toture - dodge", position = "dodge")

# range from 0 to 1
qhist(bigData, splitByCol = "y", title = "Toture - relative", position = "relative")

# color tests
# all color is defined
qhist(mtcars, "disp", horizontal = TRUE, fill = "gold", stroke = "red4")

# stacked items
qhist(mtcars, "disp", "cyl", stroke = "black", position = "stack", title = "mtcars - stack")

# raw value items
qhist(mtcars, "disp", "cyl", stroke = "black", position = "identity", title = "mtcars - identity")

# dodged items
qhist(mtcars, "disp", "cyl", stroke = "black", position = "dodge", title = "mtcars - dodge")

# range from 0 to 1
qhist(mtcars, "disp", "cyl", stroke = "black", position = "relative", title = "mtcars - relative")
