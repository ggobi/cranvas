home <- getwd()
setwd("/home/marie/Documents/cranvas/files/Marie")
source("helper.r", chdir = T)
source("axes.r", chdir = T)
source("shared.r", chdir = T)
source("../utilities/interaction.R", chdir = T)
source("../utilities/api-sketch.r", chdir = T)
rm(hbar)
rm(vbar)
setwd(home)


qfacet <- function(data, xyformula = NULL, facetformula = NULL, nrow = NULL, ncol = NULL) {
#############################
# internal helper functions #
#############################
get_bins <- function(facetvalue) {
  if (facetvalue == ".") {
    bins <- 1
  } else {
    temp <- subset(df1, select = facetvalue)[,1]
    if (!is.null(levels(temp))) {
      bins <- length(levels(temp))
    } else {
      bins <- 3 #default
    }
  }
  return(bins)
}

get_grid <- function (df1) {
  return(c(get_bins(.facetX), get_bins(.facetY)))
}

set_layout <- function (plotObj) {
  layout <- plotObj$root$gridLayout()
  layout$setColumnPreferredWidth(0, 25)
  layout$setColumnPreferredWidth(1, 15)
  for (i in 1:grid[1]) {
    layout$setColumnPreferredWidth(i + 1, 525 / grid[1])
  } 
  layout$setColumnPreferredWidth(grid[1] + 2,20)
  layout$setColumnPreferredWidth(grid[1] + 3,15)
  
  layout$setRowPreferredHeight(0, 15)
  layout$setRowPreferredHeight(1, 20)
  for (i in 1:grid[2]) {
    layout$setRowPreferredHeight(i + 1, 325/grid[2])
  }
  layout$setRowPreferredHeight(grid[2] + 2, 15)
  layout$setRowPreferredHeight(grid[2] + 3, 25)
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

############################# end internal helper functions

################################
# data processing & parameters #
################################
odata <- data
df1 <- data.frame(odata)


 ## check if an attribute exist
  has_attr <- function(attr) {
    attr %in% names(data)
  }
  
  ## parameters for the brush
  .brush.attr <- attr(data, '.brush.attr')
  if (!has_attr('.brushed')) {
    data$.brushed = FALSE
  }
  if (is.null(data$.brushed)) {
    data$.brushed <- FALSE
  }
  

if (is.null(facetformula)) {
  .facetX <- NULL
  .facetY <- NULL
} else {
  .facetX <- as.character(facetformula[[3]])
  .facetY <- as.character(facetformula[[2]])
}
#print(".facetX")
#print(.facetX)
#print(".facetY")
#print(.facetY)

if (length(xyformula) !=3) {
  stop("only xyformula of type y ~ x supported")
} else {
  .levelX <- as.character(xyformula[[3]])
  .levelY <- as.character(xyformula[[2]])
}
#print(".levelX")
#print(.levelX)
#print(".levelY")
#print(.levelY)

if( is.null(nrow) && is.null(ncol) ) {
  grid <- get_grid(df1) #ncol, nrow
} else if ( !is.null(nrow) && is.null(ncol) ) {
  grid <- c(get_grid(df1)[1], nrow)
} else if ( is.null(nrow) && !is.null(ncol) ){
  grid <- c(ncol, get_grid(df1)[2])
} else {
  grid <- c(ncol, nrow)
} 

#print("grid")
#print(grid)
dataRanges <-c(0, 1, 0, 1)

sy <- get_axisPosX(data = df1, colName = .levelX)
normalizeY <- range(sy)
sy <- (sy - min(sy))/(max(sy) - min(sy))
sx <- get_axisPosY(data = df1, colName = .levelY)
normalizeX <- range(sx)
sx <- (sx - min(sx))/(max(sx) - min(sx))

  ## parameters for datalayer
  .radius <- 2	  
  .alpha <- 1
  
  ## parameters event handling
  .startBrush <- NULL
  .endBrush <- NULL
  .brush <- FALSE

  
temp <<- NULL
############################### end data processing & parameters

##########
# layers #
##########
draw_facetlab <- function (item, painter, exposed) {
  qdrawRect(painter, xleft = 0, ybottom = 0, xright = 1, ytop = 1, fill = "grey80", stroke = "grey80")
}

back <- function(item, painter, exposed) {
print("back")
   draw_grid_with_positions_fun(painter, dataRanges, sy, sx)
  # qdrawRect(painter, xleft = 0, ybottom = 0, xright = 1, ytop = 1, fill = "grey80", stroke = "grey80") 
   #qdrawLine(painter, x = rep(c(dataRanges[1:2],NA), length(sy)), y = rep(sy, each = 3), stroke = "white")
  # qdrawLine( painter, x = rep(sx, each=3), y= rep(c(dataRanges[3:4],NA), length(sx)), stroke  = "white" )
}


dl<-function(row, col) {
  temp <-  df1[ subset(df1, select = .facetX) == sort(unique(subset(df1, select = .facetX)[[1]]))[col - 1] & subset(df1, select = .facetY) == sort(unique(subset(df1, select = .facetY)[[1]]))[row-1],]
  maxX <- max(subset(df1, select = .levelX))
  maxY <- max(subset(df1, select = .levelY))
  x <- (subset(temp, select = .levelX)[,1])/(maxX )
  y <- (subset(temp, select =.levelY)[,1] )/(maxY )
  if(has_attr(".color")){
    fill <-subset(temp, select = .color)
    stroke <- fill
  } else {
    fill <- "black"
    stroke <- "black"
  }
      

  dlpaint <- function(item, painter, exposed) {
    radius <- .radius
    if(!is.na(x[1])) {
      qdrawCircle(painter, x = x , y = y , r = radius, fill = unlist(fill), stroke = unlist(stroke) )
    }
  }
  
}

bl <- function(row, col) {
print("draw")
    df1 <-as.data.frame(odata)
    temp <-  df1[ subset(df1, select = .facetX) == sort(unique(subset(df1, select = .facetX)[[1]]))[col - 1] & subset(df1, select = .facetY) == sort(unique(subset(df1, select = .facetY)[[1]]))[row-1],]
     if(!.brush) {
       hdata <- subset(temp, (.brushed == T))
     }
     if (nrow(hdata) > 0) {
       maxX <- max(subset(df1, select = .levelX))
       maxY <- max(subset(df1, select = .levelY))
       x <- subset(hdata, select = .levelX)[,1]/maxX
       y <- subset(hdata, select = .levelY)[,1]/maxY
       fill <- .brush.attr[,".brushed.color"]   
       stroke <- .brush.attr[,".brushed.color"]   
       radius <- .radius

      blpaint <- function(item, painter, exposed) {

        qdrawCircle( painter, x = x, y = y, r = radius, fill = fill, stroke = stroke)
      }
    }
   }
 
  
 

########## end layers

###################
# draw the canvas #
###################

  plot1 <- new_plot()
  lay<-set_layout(plot1)
  ylab <-add_layer(parent = plot1, mark = text( text = .levelY, left = 0.5, bottom = .5, parent = plot1, rot = 90), col =0, row = 2, rowspan =as.integer(grid[2]))
  xlab <- add_layer(parent = plot1, mark = text( text = .levelX, left = 0.5, bottom = 0.5, parent = plot1), col = 2, row = grid[2] + 3, colspan = as.integer(grid[1]))
  
  #note: this plot is built with the idea that (eventually) user will be able to change x and y selections, but not faceting 
  #axes labels
  add_layer(parent = plot1, mark = draw_facetlab, row = 1, col = 2, colspan = grid[1])
  add_layer(parent = plot1, mark = draw_facetlab, row = 2, col = grid[1] + 2, rowspan = grid[2])
  
  # facet labels
  for (i in 1:grid[1]) {
    add_layer(plot1, text( text = tolower(get_facetlabel(.facetX, ncol, grid[1])[i]), left = 0.5, bottom = 1, valign = "top", parent = plot1), col = i + 1, row = 1)
  }
  for (i in 1:grid[2]) {
   add_layer(plot1, text( text = tolower(get_facetlabel(.facetY, nrow, grid[2])[i]), left = 0.5, bottom = .5, rot = -90, parent = plot1), col = grid[1]+ 2, row = i + 1)
  }
  
  for (i in 1:grid[2]) {
    for (j in 1:grid[1]) {
     assign(paste("back", i, j, sep = "_"), add_layer(plot1, mark = back, row = i + 1, col = j + 1, colspan = 1, rowspan = 1), pos = 1)
    }
  }
  
 ##to separate the grid backgrounds 
  for( i in 0:(grid[1] + 3)) {
    for (j in 0:(grid[2] + 3)) {
       add_layer(parent = plot1, mark = rect(left = 0, bottom = 0, height = 1, width = 1, parent = plot1, stroke = "white", fill = NULL), col = i, row =j)
       
    }
  }
  
  
   # data layers
  for( i in 1:grid[2]) {
    for ( j in 1: grid[1]) {
      qlayer(parent = plot1$root, paintFun = dl(row = i+1, col = j+1), row = i+1, col = j+1, limits = qrect(c(0,1), c(0,1)))
      qlayer(parent = plot1$root, paintFun = bl(row = i + 1, col = j + 1), limits = qrect(c(0,1), c(0,1)))
      }
   }
   
   brushlayer_2_2<-qlayer(parent=plot1$root, paintFun = bl(row = 4, col = 3), limits = qrect(c(0,1), c(0,1)))


  view <- qplotView(scene = plot1$scene)
  view$setWindowTitle(extract.formula(xyformula))
  view$setMaximumSize(plot1$size)
  view$showMaximized()
######################
# add some listeners #
######################
  if (is.mutaframe(odata)) {
    func <- function(i,j) {
      if (j == ".brushed") {
        print(j)
        print(ls(brushlayer_2_2))
        #print(.brush)
        qupdate(brushlayer_2_2$blpaint)
        
      }
    }
	
	  add_listener(odata, func)
  }

  print(view)
}
