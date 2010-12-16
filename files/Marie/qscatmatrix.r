#home <- getwd()
#source("/home/marie/Documents/cranvas/files/Marie/helper.r", chdir = T)
#source("/home/marie/Documents/cranvas/files/Marie/axes.r", chdir = T)
#source("/home/marie/Documents/cranvas/files/Marie/shared.r", chdir = T)
#source("../utilities/interaction.R", chdir = T)
#source("../utilities/api-sketch.r", chdir = T)
#rm(hbar)
#rm(vbar)
#setwd(home)


qscatmatrix <- function(data) {
#############################
# internal helper functions #
#############################
# number of rows and columns
get_nrowscols <- function(df1) {
  temp <- substr(names(df1), 1, 1)
  return(length(names(df1)) - length(temp[temp == "."]))
}


set_layout <- function (plotObj) {
	print(plot1$size$height())
  layout <- plotObj$root$gridLayout()
  layout$setColumnPreferredWidth(0, 25)
  layout$setColumnPreferredWidth(1, 15)
  for (i in 1:nrowscols) {
    layout$setColumnPreferredWidth(i + 1, (plot1$size$width() - 75) / nrowscols)
  } 
  layout$setColumnPreferredWidth(nrowscols + 2,20)
  layout$setColumnPreferredWidth(nrowscols + 3,15)
  
  layout$setRowPreferredHeight(0, 15)
  layout$setRowPreferredHeight(1, 20)
  for (i in 1:nrowscols) {
    layout$setRowPreferredHeight(i + 1, (plot1$size$height())/nrowscols)
  }
  layout$setRowPreferredHeight(nrowscols + 2, 15)
  layout$setRowPreferredHeight(nrowscols + 3, 25)
  return(layout)
}

get_facetlabel <- function(label, input, num) {
  if(!is.null(input)) {
    labels <- c(1:input)  #
  } else if (length(levels(subset(df1, select = label)[,1])) != 0) {
    labels <- levels(subset(df1, select = label)[,1])
  } else {
    labels <- sort(unique(subset(df1, select = label)[,1]))
  }
  return(labels)
}  

normalize_data <- function(x, rangeX) {
	gap <- rangeX[2] - rangeX[1]
	rangeX[1] <- rangeX[1] - 0.1 * gap
	rangeX[2] <- rangeX[2] + 0.1 * gap
	return (( x - rangeX[1])/(rangeX[2] - rangeX[1]))
	
}
############################# end internal helper functions

################################
# data processing & parameters #
################################
odata <- data

#print(paste("data attribute at import:", brush_attr(data, ".brushed.color")))
#print("------")
#print(paste("odata attribute at import:", brush_attr(odata, ".brushed.color")))
#print("-----1")
df1 <- data.frame(odata)

## check if an attribute exist
has_attr <- function(attr) {
  attr %in% names(data)
}
  
## parameters for the brush
.brush.attr <- attr(data, '.brush.attr')

if (!has_attr('.brushed')) {
  data$.brushed <- FALSE
}

if (is.null(data$.brushed)) {
  data$.brushed <- FALSE
}
  


dataRanges <-c(0, 1, 0, 1)
nrowscols <<- get_nrowscols(odata)

## parameters for datalayer
.radius <- 2	  
.alpha <- 1
  
## parameters event handling
.startBrush <- NULL
.endBrush <- NULL
.brush <- FALSE

temp <- NULL

############################### end data processing & parameters

##########
# layers #
##########
draw_facetlab <- function (item, painter, exposed) {
  qdrawRect(painter, xleft = 0, ybottom = 0, xright = 1, ytop = 1, fill = "grey80", stroke = "grey80")
}

back <- function(item, painter, exposed) {
   draw_grid_with_positions_fun(painter, dataRanges, c(0,1), c(0,1))
}


dl<-function(row, col) {
  x <- odata[,row ]
  y <- odata[,col]
#  print(range(x))
#  print(range(y))
  
  if(class(x) == "numeric") {
    x <- normalize_data(x, range(x))
  }
  
  if(class(y) == "numeric") {
	  y <- normalize_data(y, range(y))
  }
  if(has_attr(".color")){
    fill <- odata[colnames(odata) == ".color"][,1]
    stroke <- fill
  } else {
    fill <- "black"
    stroke <- "black"
  }
      
  # drawing call
  dlpaint <- function(item, painter, exposed) {
    radius <- .radius
    if(!is.na(x[1])) {
		if (colnames(odata)[col ] != colnames(odata)[row]) {
      qdrawCircle(painter, x = x , y = y , r = radius, fill = unlist(fill), stroke = unlist(stroke) )
    	} else {
			qdrawText(painter, text = colnames(odata)[row], x = .5, y = .5)
		}
	}
  }
}

bl <- function(row, col) {
	.brush <- T
	if(.brush & (colnames(odata)[row] != colnames(odata)[col])){
		df1 <- odata
# drawing call
		blpaint <- function(item, painter, exposed) {
			fill <- brush_attr(odata, ".brushed.color")
			stroke <- fill
			radius <-.radius
			df1[,row] <- normalize_data(df1[,row], range(df1[,row]))
			df1[,col] <- normalize_data(df1[,col], range(df1[,col]))
			df1 <- df1[df1$.brushed,]
			x <- df1[, row]
			y <- df1[,col]				
			qdrawCircle(painter, x = x, y = y, r = radius, fill = unlist(fill), stroke = unlist(stroke))
		}
	}
}

########## end layers

###################
# draw the canvas #
###################

plot1 <- new_plot(width = 600, height = 600)
lay<-set_layout(plot1)
  
#note: this plot is built with the idea that (eventually) user will be able to change x and y selections, but not faceting 
#axes labels
add_layer(parent = plot1, mark = draw_facetlab, row = 1, col = 2, colspan = nrowscols)
add_layer(parent = plot1, mark = draw_facetlab, row = 2, col = nrowscols + 2, rowspan = nrowscols)
  
# facet labels
for (i in 1:nrowscols) {
  add_layer(plot1, text( text = names(odata)[i], left = 0.5, bottom = 1, valign = "top", parent = plot1, pointsize = 10), col = i + 1, row = 1)
}

for (i in 1:nrowscols) {
   add_layer(plot1, text( text = names(odata)[i], left = 0.75, bottom = .5, rot = -90, valign = "top", parent = plot1, pointsize = 9), col = nrowscols + 2, row = i + 1)
}
  
# background, with gridlines
for (i in 1:nrowscols) {
  for (j in 1:nrowscols) {
    add_layer(plot1, mark = back, row = i + 1, col = j + 1, colspan = 1, rowspan = 1)
  }
}
  
# to separate the grid backgrounds 
for( i in 0:(nrowscols + 3)) {
  for (j in 0:(nrowscols + 3)) {
    add_layer(parent = plot1, mark = rect(left = 0, bottom = 0, height = 1, width = 1, parent = plot1, stroke = "white", fill = NULL), col = i, row =j)
  }
}
  
# data layers
for( i in 1:nrowscols) {
  for ( j in 1: nrowscols) {
    qlayer(parent = plot1$root, paintFun = dl(row = i, col = j), row = i+1, col = j+1, limits = qrect(c(0,1), c(0,1)))
  }
}

# brush layers
for( i in 1:nrowscols) {
	for(j in 1:nrowscols) {
		  blayer <- qlayer(parent = plot1$root, paintFun = bl(row = i, col = j), row = i + 1, col = j + 1, limits = qrect(c(0,1), c(0,1)))
	}
}

   
view <- qplotView(scene = plot1$scene)
#view$setMaximumSize(plot1$size)
#view$showMaximized()

######################
# add some listeners #
######################
if (is.mutaframe(data)) {
  	func <- function(i,j) {
 		qupdate(blayer)
	}
  	add_listener(data, func)
}


print(view)

}
