#source("../cranvas/load.r")
#source("api_0.1-2.R")
source("helper.r")
source("axes.r")
source("shared.r")
source("../utilities/interaction.R")
#rm(hbar)
#rm(vbar)

#' Draw a scatterplot
#' 
#' @param data data.frame source
#' @param form formula in format y ~x which designates the axis
#' @param main main title for the plot
#' @param labeled whether axes should be labeled

qscatter <- function (data, form, main = NULL, labeled = TRUE) {
#############################
# internal helper functions #
#############################

############################# end internal helper functions

################################
# data processing & parameters #
################################

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
  
  print(head(as.data.frame(data)))
  if (length(form) != 3) {
    stop("invalid formula, requires y ~ x format")
  } else {
    .levelX <- as.character( form[[3]] )
    .levelY <- as.character(form[[2]])
  }
  
  ## local copy of original data
  odata <- data
  
  ## transform the data
  df <- data.frame(data)

  ## parameters for dataRanges
  xlab <- NULL
  ylab <- NULL
  
  ## labels
  ylabels <- NULL
  if (labeled) {
    yid <- find_yid(data = df, colName = as.character(.levelY))
  } else {
    yid <- NA
  }
  
  if (!is.na(yid[1]) ) {
      ylabels <- get_axisPosY(data = df, colName = .levelY)
  }
  
  xlabels <- NULL
  if (labeled) {
    xid <- find_xid(data = df, colName = as.character(.levelX))
  } else {
    xid <- NA
  }
  
  if (!is.na(xid[1])) {
      xlabels <- get_axisPosY(data = df, colName = .levelX)
  }

  ## parameters for all layers
  if (labeled) {
  dataRanges <- c(
    make_data_ranges(range(subset(df, select = .levelX))),
    make_data_ranges(range(subset(df, select = .levelY))))
 
  windowRanges <- make_window_ranges(dataRanges, xlab, ylab,
    ytickmarks=ylabels, xtickmarks = xlabels, main=main)
  } else {
    dataRanges <- c(range(subset(df, select = .levelX)),
                    range(subset(df, select = .levelY)))
    windowRanges <- dataRanges
  }
  
  lims <- qrect(windowRanges[c(1,2)], windowRanges[c(3,4)])

  ## parameters for bglayer
  sy <- get_axisPosX(data = df, colName = .levelX)
  sx <- get_axisPosY(data = df, colName = .levelY)

  ## parameters for datalayer
  .radius <- 2	  
  .alpha <- 1

  ## parameters event handling
  .startBrush <- NULL
  .endBrush <- NULL
  .brush <- FALSE


################################ end data processing & parameters

##########
# layers #
##########
coords <- function(item, painter, exposed) {
 
  # grey background with grid lines
  draw_grid_with_positions_fun(painter, dataRanges, sy, sx)
    
  # labels as appropriate
  if (!is.na(xid[1])) {
    labels <- get_axisPosX(data = df, colName = .levelX)
    print("x axis labels")
    print(labels)
    draw_x_axes_with_labels_fun(painter, dataRanges, 
      axisLabel=sy, labelHoriPos=sy, name=xlab)
  } else {
    draw_x_axes_with_labels_fun(painter, dataRanges,
      axisLabel=rep("",length(sy)), labelHoriPos=sy,
      name=xlab)
  }

  if (!is.na(yid[1])) {
    labels <- get_axisPosY(data = df, colName = .levelY)
    draw_y_axes_with_labels_fun(painter, dataRanges, 
      axisLabel=sx, labelVertPos=sx, name=ylab)
  } else {
    draw_y_axes_with_labels_fun(painter, dataRanges, 
       axisLabel=rep("",length(sx)), labelVertPos=sx,
       name=ylab)
  }

}

scatter.all <- function(item, painter, exposed) {
  x <- subset(df, select = .levelX)[,1]
  y <- subset(df, select = .levelY)[,1]
  if (has_attr(".color")) {
  	fill <- odata$.color
  	stroke <- odata$.color
  } else {
    fill <- "black"
    stroke <- "black"
  }
  radius <- .radius
  qdrawCircle(painter, x = x, y = y, r = radius, fill = fill, stroke = stroke)  
}

brush.draw <- function(item, painter, exposed) {
  df <- as.data.frame(odata)
  if(!.brush) {
    hdata <- subset(df, (.brushed == T))
  }
  
  if (nrow(hdata) > 0) {
    x <- subset(hdata, select = .levelX)[,1]
    y <- subset(hdata, select = .levelY)[,1]
    fill <- .brush.attr[,".brushed.color"]   
    stroke <- .brush.attr[,".brushed.color"]   
    radius <- .radius
    
    qdrawCircle( painter, x = x, y = y, r = radius, fill = fill, stroke = stroke)
  }
}
########## end layers

####################
## event handlers ##
####################

keyPressFun <- function(item, event, ...) {
    key <- event$key()

    if (key == Qt$Qt$Key_Up) {        # arrow up
		.radius <<- .radius+1
        qupdate(datalayer$layer)
		qupdate(brushlayer$layer)
    } else if (key == Qt$Qt$Key_Down & .radius > 0) {        # arrow down
        .radius <<- .radius - 1
        qupdate(datalayer$layer)
		qupdate(datalayer$layer)
    } else if (key == Qt$Qt$Key_Right & .alpha < 1) {        # arrow right
	# increase alpha blending
        .alpha <<- .alpha + 0.01
        datalayer$layer$setOpacity(.alpha)
		brushlayer$layer$setOpacity(.alpha)
        qupdate(datalayer$layer)
		qupdate(datalayer$layer)

    } else if (key == Qt$Qt$Key_Left & .alpha > 0) {        # arrow left
	# decrease alpha blending
        .alpha <<- .alpha - 0.01
        datalayer$layer$setOpacity(.alpha)
		brushlayer$layer$setOpacity(.alpha)
        qupdate(datalayer$layer)
    }
	
		
  }  
########## end event handlers

###################
# draw the canvas #
###################

  plot1 <- new_plot()
  bglayer <- add_layer(parent = plot1, mark = coords, userlimits = lims)
  datalayer <- add_layer(parent = plot1, mark = scatter.all, keyPressFun = keyPressFun, userlimits = lims)
  brushlayer <- add_layer(parent = plot1, mark = brush.draw, userlimits = lims)
  view <- qplotView(scene = plot1$scene)
  view$setWindowTitle(extract.formula(form))
 # view$setMaximumSize(plot1$size)

######################
# add some listeners #
######################
  if (is.mutaframe(odata)) {
    func <- function(i,j) {
      if (j == ".brushed") {
        qupdate(brushlayer$layer)
      }
    }
	
	  add_listener(odata, func)
  }

  print(view) 

}
