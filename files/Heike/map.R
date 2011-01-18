require(plumbr)

myvarsummary <- function(x) {
	if (is.factor(x) || is.character(x)) return(names(sort(table(x), decreasing=TRUE))[1])
	if (is.logical(x)) return(any(x, na.rm=TRUE))
	
	return(mean(x))
}

mysummary <- function(x) {
	ldply(x, myvarsummary)
}

##' Interactive Maps.
##' Create an interactive map from qmutaframe
##'
##' @param data a mutaframe which is typically built upon a data frame
##' along with several row attributes
##' @param latitude spatial x variable
##' @param longitude spatial y variable
##' @param group grouping variable for polygons
##' @param colour fill colour of the polygons
##' @param ...
##' @return NULL
##' @author Heike Hofmann
##' @export
##' @example cranvas/inst/examples/maps-ex.R
qtmap <- function(data, longitude, latitude, group, by.x=NULL, label=NULL, labeldata=NULL, by.y=by.x,  colour="grey30", main=NULL, ...) {
  ## check if an attribute exist
  has_attr = function(attr) {
      attr %in% names(data)
  }
	xname <- as.character(substitute(longitude))
	yname <- as.character(substitute(latitude))
	gname <- as.character(substitute(group))
	lname <- as.character(substitute(label))
	
	if (!xname %in% attr(data,"col.names")) stop(paste("object",xname,"not found"))
	if (!yname %in% attr(data,"col.names")) stop(paste("object",yname,"not found"))
	if (!gname %in% attr(data,"col.names")) stop(paste("object",gname,"not found"))

	x <- data[ ,xname]
	y <- data[ ,yname]
	group <- data[ ,gname]
	.groupsdata <- ddply(data.frame(data),.(group), mysummary)
	.groupsdata <- cast(.groupsdata, group ~ .id, value="V1")
	names(.groupsdata)[1] <- "ID"
	.groupsdata$.color <- "grey30"
	if (!(".brushed" %in% names(.groupsdata))) .groupsdata$.brushed <- FALSE
	.recalcbrushed <- FALSE
	.recalclbrushed <- FALSE
	# extended infostring - shift turns this to TRUE
	.extended <- FALSE
	
  ## parameters for the brush
  .brush.attr = attr(data, '.brush.attr')
  if (!has_attr('.brushed')) data$.brushed <- FALSE

	# by.x and by.y connect datasets data and labeldata
	# check that connection works
	if (!is.null(by.x)) {
		# assume they are the same, if only one is specified
		# labeldata is a subset of the groups - i.e. only one value for each group on each variable
		if (!is.mutaframe(labeldata)) labeldata <- qmutaframe(labeldata)
		
		xid <- as.character(substitute(by.x))
		yid <- as.character(substitute(by.y))
		idx <- setdiff(names(labeldata), c(".color", ".brushed"))
		.groupsdata <- merge(.groupsdata, data.frame(labeldata)[,idx], by.x=xid, by.y=yid, all.x=TRUE)
		
		cname <- as.character(substitute(colour))
		.colored  <- cname %in% attr(labeldata,"col.names")
#		browser()
		if (.colored) .groupsdata$.color <- as.character(.groupsdata[,cname])
	}
#print(summary(.groupsdata))
#	.colored <- has_attr('.color')

  if (is.null(main)) .df.title <- paste("Map of",deparse(substitute(data)))
#  xlab <- find_x_label(xdata)
#  ylab <- find_y_label(xdata)

  dataRanges <- c(
    make_data_ranges(range(x)),
    make_data_ranges(range(y)))

  # space in window around plot (margins in base R)
  # this space depends on the labels needed on the left
  # find out about these first:


  windowRanges <- make_window_ranges(dataRanges, "", "",
    ytickmarks="", main=.df.title)

  lims <- qrect(windowRanges[c(1,2)], windowRanges[c(3,4)])


  draw <- function(item, painter, exposed) {
  # extract data at level .level and draw
#print("mdraw: full mosaic drawn")
##print(summary(xdata))
#		if (.recalc) recalc()


		for (i in .groupsdata$ID) {
			xx <- x[group==i] 
			yy <- y[group==i]
			qdrawPolygon(painter,
				xx,
				yy,
				stroke="grey80",
				fill=.groupsdata$.color[i]
			)
		}

    add_title_fun(painter, dataRanges, title=.df.title)
  }

  # Brushing -----------------------------------------------------------------
  .startBrush <- NULL
  .endBrush <- NULL
  .brush <- FALSE

  drawBrush <- function(item, painter, exposed) {
    left = min(.startBrush[1], .endBrush[1])
    right = max(.startBrush[1], .endBrush[1])
    top = max(.startBrush[2], .endBrush[2])
    bottom = min(.startBrush[2], .endBrush[2])

    qdrawRect(painter, left, bottom, right, top,
      fill=rgb(0,0,0,alpha=0.3), stroke="black")
  }

	recalcbrushed <- function() {
		for (i in .groupsdata$ID) {
			brushed <- data$.brushed[group == i]
			.groupsdata$.brushed[i] <<- any(brushed)
			
		}
		.recalcbrushed <<- FALSE

		setSelectedLabel()
	}

	recalclbrushed <- function() {
	for (i in unique(.groupsdata[,xid])) {
			brushed <- labeldata[labeldata[,yid]==i, ".brushed"]
			.groupsdata$.brushed[.groupsdata[,xid]==i] <<- any(brushed)			
		}
		.recalclbrushed <<- FALSE

		setSelected()
	}

  brushing_draw <- function(item, painter, exposed, ...) {
		if (.recalcbrushed) recalcbrushed()
		if (.recalclbrushed) recalclbrushed()
print("brushing_draw")
    if (!is.null(.endBrush)) {
      drawBrush(item, painter, exposed)
    }

		bgroups <- subset(.groupsdata, .brushed == TRUE)
		if (nrow(bgroups) == 0) return()
#print(bgroups)
		brushcolor <- brush_attr(data, ".brushed.color")
		
		for (i in bgroups$ID) {
			xx <- x[group==i] 
			yy <- y[group==i]
			qdrawPolygon(painter,
				xx,
				yy,
				stroke="grey80",
				fill=brushcolor
			)
			
		}
  }

  brushing_mouse_press <- function(item, event, ...) {
 	#print("brushing_mouse_press")
   .brush <<- TRUE
    if (is.null(.startBrush)) {
      .startBrush <<- as.numeric(event$pos())
      .endBrush <<- as.numeric(event$pos())
    }

    setHiliting()
    qupdate(brushing_layer)
  }

  brushing_mouse_move <- function(item, event, ...) {
 	#print("brushing_mouse_move")
    .endBrush <<- as.numeric(event$pos())

    setHiliting()
    qupdate(brushing_layer)
  }

  brushing_mouse_release <- function(item, event, ...) {
 	#print("brushing_mouse_release")
    .endBrush <<- as.numeric(event$pos())
    setHiliting()
    qupdate(brushing_layer)


    .brush <<- FALSE


    .startBrush <<- NULL
    .endBrush <<- NULL

	  setSelected()
  }

  setHiliting <- function() {
    left = min(.startBrush[1], .endBrush[1])
    right = max(.startBrush[1], .endBrush[1])+1e-8
    top = max(.startBrush[2], .endBrush[2])+1e-8
    bottom = min(.startBrush[2], .endBrush[2])

 		rect = qrect(matrix(c(left, bottom, right, top), 2, byrow = TRUE))
    hits = datalayer$locate(rect) + 1
		.groupsdata$.brushed <<- FALSE
		.groupsdata$.brushed[hits] <<- TRUE
  }

  setSelected <- function() {
print("set selected")
	# propagate highlighting to the data set and other plots
#browser()
#		brushed <- data$.brushed
#		brushed <- FALSE
		
		bdata <- subset(.groupsdata, .brushed == TRUE)		
		brushed <- group %in% bdata$ID

		if (any(data$.brushed != brushed))	data$.brushed <- brushed

		if (!is.null(labeldata)) setSelectedLabel()
  }

	setSelectedLabel <- function () {
	print("set selected labeldata")
		bdata <- subset(.groupsdata, .brushed == TRUE)		
		brushed <- labeldata[, yid] %in% bdata[, xid]
			
		if (any(labeldata$.brushed != brushed)) labeldata$.brushed <- brushed
	}

  # Key board events ---------------------------------------------------------

  keyPressFun <- function(item, event, ...) {
		if (event$key() == Qt$Qt$Key_Shift) .extended <<- !.extended
		print("extended:") 
		print(.extended)
  }


  # Display category information on hover (query) ----------------------------
  .queryPos <- NULL

  query_draw <- function(item, painter, exposed, ...) {
    # Don't draw when brushing
    if (.brush) return()
    if (is.null(.queryPos)) return()
    xpos <- .queryPos[1]
    ypos <- .queryPos[2]
 		rect = qrect(matrix(c(xpos,ypos,xpos+1e-4, ypos+1e-4), 2, byrow = TRUE))
    hits = datalayer$locate(rect) + 1

		info <- .groupsdata[hits,]

    # Nothing under mouse
    if (is.null(info)) return()
    if (nrow(info) == 0) return()

		infostring = paste(lname, .groupsdata[hits, lname],collapse="\n", sep=":")
		if (.extended) {
		  idx <- setdiff(names(.groupsdata), c("order", ".color", ".brushed", xname, yname, gname))
      infodata <- as.character(unlist(info[1,idx]))
      infostring <- paste(idx, infodata,collapse="\n", sep=":")
		}


		brushcolor <- brush_attr(data, ".brushed.color")
		for (i in info$ID) {
			xx <- x[group==i] 
			yy <- y[group==i]
			qdrawPolygon(painter,
				xx,
				yy,
				stroke=brushcolor,
				fill=NULL
			)		
		}
    qstrokeColor(painter) <- brushcolor
    qdrawText(painter, infostring, xpos, ypos, valign="top", halign="left")
  }

  query_hover <- function(item, event, ...) {
    if (.brush) return()

    .queryPos <<- as.numeric(event$pos())
    qupdate(querylayer)
  }

  query_hover_leave <- function(item, event, ...) {
    .queryPos <<- NULL
    qupdate(querylayer)
  }

	coords <- function(item, painter, exposed) {
	}
	
  scene = qscene()
  bglayer = qlayer(scene, coords, limits = lims, clip = FALSE)
  datalayer = qlayer(scene, draw, 
    limits = lims, clip = FALSE)
  brushing_layer = qlayer(scene, brushing_draw,
		mousePressFun = brushing_mouse_press, mouseMoveFun = brushing_mouse_move,
    mouseReleaseFun = brushing_mouse_release,
    keyPressFun=keyPressFun,
    limits = lims, clip = FALSE)
  querylayer = qlayer(scene, query_draw,    
  	hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave,
  	limits = lims, clip = FALSE)



	## update the brush layer in case of any modifications to the mutaframe
	add_listener(data, function(i, j) {
		switch(j, 
			.brushed = { 
print("addlistener: brushed")
									.recalcbrushed <<- TRUE
									qupdate(brushing_layer) },
	    .color = { 
	    					 qupdate(datalayer)
	    					 qupdate(brushing_layer)
	    				 }
		)
	})

	add_listener(labeldata, function(i, j) {
		switch(j, 
			.brushed = { 
print("addlistener: labeldata brushed")
									.recalclbrushed <<- TRUE
									qupdate(brushing_layer) },
	    .color = { 
	    					 qupdate(datalayer)
	    					 qupdate(brushing_layer)
	    				 }
		)
	})


	## update the brush layer if brush attributes change
	add_listener(.brush.attr, function(i, j) {
			qupdate(brushing_layer)
	})

  qplotView(scene = scene)
}

