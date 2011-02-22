myvarsummary <- function(x) {
	if (is.factor(x) || is.character(x)) return(names(sort(table(x), decreasing=TRUE))[1])
	if (is.logical(x)) return(any(x, na.rm=TRUE))

	return(mean(x))
}

mysummary <- function(x) {
	ldply(x, myvarsummary)
}

scale_color <- function(colour, value = colour, na.color = 0) {
	if (is.numeric(colour)) {
	# assume grey colour scheme
		cmin <- min(colour, na.rm=T)
		cmax <- max(colour, na.rm=T)
		grey <- (value-cmin)/(cmax-cmin)
		grey <- pmin(grey, 1)
		grey <- pmax(grey, 0)
		nas <- is.na(grey)
		grey[nas] <- na.color
		return(rgb(grey,grey,grey))
	}

	print(paste("colour not implemented for type", mode(colour)))
}


##' Interactive Maps.
##' Create an interactive map from qdata
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
qtmap <- function(data, longitude, latitude, group, by.x=NULL, label=group, labeldata=NULL, by.y=by.x,  colour=NULL, main=NULL, ...) {
  ## check if an attribute exist
#  browser()
  arguments <- as.list(match.call()[-1])
 	df.data <- data.frame(data)

	x <- eval(arguments$longitude, df.data)
	y <- eval(arguments$latitude, df.data)
	group <- eval(arguments$group, df.data)
	if (is.null(labeldata))
		label <- unique(eval(arguments$label, df.data))


	.groupsdata <- ddply(df.data,.(group), mysummary)
	.groupsdata <- cast(.groupsdata, group ~ .id, value="V1")
#	.groupsdata$ID <- 1:nrow(.groupsdata)
	names(.groupsdata)[1] <- "ID"
	.groupsdata$.color <- "grey30"
	if (!(".brushed" %in% names(.groupsdata))) .groupsdata$.brushed <- FALSE
	.recalcbrushed <- FALSE
	.recalclbrushed <- FALSE
	# extended infostring - shift turns this to TRUE
	.extended <- FALSE
	.legendspace <- 0

  ## parameters for the brush
  .brush.attr = attr(data, '.brush.attr')
	

	# by.x and by.y connect datasets data and labeldata
	# check that connection works
	if (!is.null(by.x)) {
		# assume they are the same, if only one is specified
		# labeldata is a subset of the groups - i.e. only one value for each group on each variable
		if (!is.mutaframe(labeldata)) labeldata <- qdata(labeldata)


		xid <- as.character(substitute(by.x))
		yid <- as.character(substitute(by.y))
		idx <- setdiff(names(labeldata), c(".color", ".brushed"))
		df.labeldata <- data.frame(labeldata)
		.groupsdata <- merge(.groupsdata, df.labeldata[,idx], by.x=xid, by.y=yid, all.x=TRUE)
		label <- eval(arguments$label, .groupsdata)

#		browser()
		if (!is.null(arguments$colour)) {
			.groupsdata$.color <- scale_color(eval(arguments$colour, .groupsdata))
			.legendspace <- nchar(deparse(arguments$colour)) * 0.015 * diff(range(x)) # qstrWidth(p, deparse(arguments$colour)) #p is painter object - don't know yet
		}
	}

  if (is.null(main)) .df.title <- paste("Map of",deparse(substitute(data)))
#  xlab <- find_x_label(xdata)
#  ylab <- find_y_label(xdata)

  dataRanges <- c(
    make_data_ranges(c(min(x), max(x)+.legendspace)),
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


		for (j in 1:length(.groupsdata$ID)) {
			i <- .groupsdata$ID[j]
			xx <- x[group==i]
			yy <- y[group==i]
			qdrawPolygon(painter,
				xx,
				yy,
				stroke="grey80",
				fill=.groupsdata$.color[j]
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
#browser()
		for (i in 1:length(.groupsdata$ID)) {
			brushed <- data$.brushed[group == .groupsdata$ID[i]]
			.groupsdata$.brushed[i] <<- any(brushed)

		}
		.recalcbrushed <<- FALSE

		if (!is.null(labeldata)) setSelectedLabel()
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
		if (!is.null(labeldata) && (.recalclbrushed)) recalclbrushed()

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
		
#print("brushing_draw")
    if (!is.null(.endBrush)) {
      drawBrush(item, painter, exposed)
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
#print("set selected")
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
	#print("set selected labeldata")
		bdata <- subset(.groupsdata, .brushed == TRUE)
		if (is.null(yid)) return()
		if (is.null(xid)) return()
		
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

		infostring = paste(deparse(arguments$label), label[hits],collapse="\n", sep=":")
		if (.extended) {
#browser()
		  idx <- setdiff(names(.groupsdata), c("order", ".color", ".brushed", as.character(arguments$longitude), as.character(arguments$latitude), as.character(arguments$group)))
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

    bgwidth = qstrWidth(painter, infostring)
    bgheight = qstrHeight(painter, infostring)

#    qdrawText(painter, infostring, xpos, ypos, valign="top", halign="left")
    
    
		## adjust drawing directions when close to the boundary
		hflag = windowRanges[2] - xpos > bgwidth
		vflag = ypos - windowRanges[3] > bgheight
		qdrawRect(painter, xpos, ypos,
							xpos + ifelse(hflag, 1, -1) * bgwidth,
							ypos + ifelse(vflag, -1, 1) * bgheight,
							stroke = rgb(1, 1, 1, 0.5), fill = rgb(1, 1, 1, 0.5))

		qstrokeColor(painter) = brush_attr(data, '.label.color')
    qdrawText(painter, infostring, xpos, ypos,
    	halign = ifelse(hflag, "left", "right"),
      valign = ifelse(vflag, "top", "bottom"))
    
    
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

  # Display legend information for colour ----------------------------

  legend_draw <- function(item, painter, exposed, ...) {
#print("legend_draw")
		if (is.null(arguments$colour)) return()

		xpos = max(x)
		ypos = (max(y)+ min(y))/2
#browser()
    qstrokeColor(painter) <- 'black'
    qdrawText(painter, deparse(arguments$colour), xpos, ypos, valign="top", halign="left")
		fontHeight <- qstrHeight(painter, deparse(arguments$colour))

		# create a set of rectangles
		r0 <- 0.05 * c(0,0,diff(range(x)), diff(range(y))) # initial rectangle
		ypos <- ypos - 3*qstrHeight(painter, deparse(arguments$colour))
		r0 <- r0 + c(xpos, ypos)

		col <- eval(arguments$colour, .groupsdata)
  	d <- options()$str$digits.d
#  	browser()
		qcol <- round(quantile(col, probs=c(0,.25,.5,.75,1), na.rm=T, names=FALSE), d)

		for (i in 1:length(qcol)) {
			qdrawRect(painter, r0[1], r0[2],r0[3],r0[4],
      fill=scale_color(col, qcol[i]), stroke="black")
			qdrawText(painter, as.character(qcol[i]), xpos+ 1.5*(r0[3]-r0[1]), ypos+fontHeight, valign="top", halign="left")
			ypos <- ypos - 1.5*fontHeight
			r0[2] <- r0[2] - 1.5*fontHeight
			r0[4] <- r0[4] - 1.5*fontHeight
		}

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
  legendlayer = qlayer(scene, legend_draw,
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
if(!is.null(labeldata)) {
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
}

	## update the brush layer if brush attributes change
# commented out for now to get the code to run, but we do need the capability
#	add_listener(.brush.attr, function(i, j) {
#			qupdate(brushing_layer)
#	})

  qplotView(scene = scene)
}

