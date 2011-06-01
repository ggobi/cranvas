#' Draw a scatterplot
#'
#' arrow up/down: in-/de-crease size of points
#' arrow left/right: de-/in-crease alpha level (starts at alpha=1 by default)
#' Key 'z' toggle zoom on/off (default is off): mouse click & drag will specify a zoom window, reset to default window by click/no drag
#' Key 'x' toggle focal zoom on/off (default is off): mouse click & drag will specify a zoom window, zoom out by pressing shift key
#' Key 'r' resets data range to original scale
#' @param data mutaframe data to use
#' @param x which designates variable displayed on the horizontal axis
#' @param y which designates variable displayed on the vertical axis
#' @param main main title for the plot
#' @param labeled whether axes should be labeled
#' @param size point size
#' @param alpha transparency level, 1=completely opaque
#' @param xlim = c(min, max) user specifed data range for the x axis, by default range(x)
#' @param ylim = c(min, max) user specifed data range for the y axis, by default range(y)
#' @param xlab label on horizontal axis, default is name of x variable
#' @param ylab label on vertical axis, default is name of y variable
#' @param cache boolean to turn cache on for layers, defaults to TRUE
#' @example cranvas/inst/examples/qscat-ex.R

qscatter <- function(data, x, y, aspect.ratio = NULL, main = NULL,
                     labeled = TRUE, size = 2, alpha = 1, xlim=NULL, 
                     ylim=NULL, xlab=NULL, ylab=NULL, cache = T, ...)
{
    stopifnot(is.mutaframe(data))
    
    ################################
    # data processing & parameters #
    ################################

    arguments <- as.list(match.call()[-1])

    ## transform the data
    df <- data.frame(data)
    x <- eval(arguments$x, df)
    y <- eval(arguments$y, df)

		stopifnot(!is.null(x), !is.null(y))
		stopifnot(length(x) > 1, length(y) > 1, length(x) != length(y))

		if (is.null(xlim)) xlim <- range(x, na.rm=T)
		if (is.null(ylim)) ylim <- range(y, na.rm=T)
		

    ## parameters for dataRanges
    if(is.null(xlab)) xlab <- deparse(arguments$x)
    if(is.null(ylab)) ylab <- deparse(arguments$y)
#browser()
		values_out_of_plotting_range <- FALSE

    ## parameters for all layers
		dataRanges <- c(make_data_ranges(xlim), 
									  make_data_ranges(ylim))

    lims <- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])

    ## parameters for datalayer
    .radius <- size
    .alpha <- alpha

    ## parameters event handling
    .startBrush <- NULL
    .endBrush <- NULL
    ## whether in the brush mode
    .brush <- TRUE
    ## mouse position
    .bpos <- c(NA, NA)
    ## drag start
    .bstart <- c(NA, NA)
    ## move brush?
    .bmove <- TRUE
    ## brush range: horizontal and vertical
    .brange <- c(diff(dataRanges[c(1, 2)]), diff(dataRanges[c(3, 4)]))/30

    n <- nrow(data)

		## zooming on?
		zoom <- FALSE
		.zstart <- NULL
		.zstop <- NULL

		zoom_focal <- FALSE

    ################################ end data processing & parameters

    ##########
    # layers #
    ##########


    xaxis <- function(item, painter, exposed) {
				sx <- .axis.loc(dataRanges[1:2])
				xlabels <- rep("", length(sx))
				
				if (labeled) xlabels <- sx 

				draw_x_axes_with_labels_fun(painter, c(dataRanges[1:2],1,5), axisLabel = xlabels,
																		labelHoriPos = sx, name = xlab)

    }

    yaxis <- function(item, painter, exposed) {
				sy <- .axis.loc(dataRanges[3:4])
				ylabels <- rep("", length(sy))
				
				if (labeled) ylabels <- sy 
        
				draw_y_axes_with_labels_fun(painter, c(1,5, dataRanges[3:4]), axisLabel = ylabels,
																		labelVertPos = sy, name = ylab)
    }

    grid <- function(item, painter, exposed) {
				sx <- .axis.loc(dataRanges[1:2])
				sy <- .axis.loc(dataRanges[3:4])

        # grey background with grid lines
        draw_grid_with_positions_fun(painter, dataRanges, sx, sy)
    }


    scatter.all <- function(item, painter, exposed) {
        fill <- data$.color
        stroke <- data$.color
        df <- data.frame(data)
        x <- eval(arguments$x, df)
        y <- eval(arguments$y, df)

        radius <- .radius
        qdrawCircle(painter, x = x, y = y, r = radius, fill = fill,
                    stroke = stroke)

				if (values_out_of_plotting_range) {
					if (any(x < dataRanges[1]))
						qdrawSegment(painter, x0 = dataRanges[1], x1=dataRanges[1], 
															 y0=dataRanges[3], y1=dataRanges[4],
															 stroke = "red")
					if (any(x > dataRanges[2]))
						qdrawSegment(painter, x0 = dataRanges[2], x1=dataRanges[2], 
															 y0=dataRanges[3], y1=dataRanges[4],
															 stroke = "red")
					if (any(y < dataRanges[3]))
						qdrawSegment(painter, x0 = dataRanges[1], x1=dataRanges[2], 
															 y0=dataRanges[3], y1=dataRanges[3],
															 stroke = "red")
					if (any(y > dataRanges[4]))
						qdrawSegment(painter, x0 = dataRanges[1], x1=dataRanges[2], 
															 y0=dataRanges[4], y1=dataRanges[4],
															 stroke = "red")
				}
    }

    brush_draw <- function(item, painter, exposed) {
        df <- as.data.frame(data)
        .brushed <- data$.brushed
        if (.brush) {
            if (!any(is.na(.bpos))) {
                qlineWidth(painter) = brush(data)$size
                ##qdash(painter)=c(1,3,1,3)
                qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - .brange[2],
                          .bpos[1] + .brange[1], .bpos[2] + .brange[2],
                          stroke = brush(data)$color)
            }

            hdata <- subset(df, .brushed)

            if (nrow(hdata) > 0) {
                ## draw the brush rectangle
                if (!any(is.na(.bpos))) {
                  qlineWidth(painter) = brush(data)$size
                  ##qdash(painter)=c(1,3,1,3)
                  qdrawRect(painter, .bpos[1] - .brange[1],
                            .bpos[2] - .brange[2], .bpos[1] + .brange[1],
                            .bpos[2] + .brange[2], stroke = brush(data)$color)
                }
                ## (re)draw brushed data points
                brushx <- eval(arguments$x, hdata)
                brushy <- eval(arguments$y, hdata)
                fill <- brush(data)$color
                stroke <- brush(data)$color
                radius <- .radius

                qdrawCircle(painter, x = brushx, y = brushy, r = radius,
                            fill = fill, stroke = stroke)
            }
        }
    }
    ########## end layers

    ####################
    ## event handlers ##
    ####################

    keyPressFun <- function(item, event, ...) {
		# arrow up/down: in/de-crease point size
		# arrow left/right: de/in-crease alpha level
		# z toggle zoom on/off (default is off): mouse click & drag will specify a zoom window
        key <- event$key()

        if (key == Qt$Qt$Key_Up) {
            # arrow up
            .radius <<- .radius + 1
            qupdate(datalayer)
            qupdate(brushlayer)
        }
        else if (key == Qt$Qt$Key_Down & .radius > 0) {
            # arrow down
            .radius <<- .radius - 1
            qupdate(datalayer)
            qupdate(datalayer)
        }
        else if (key == Qt$Qt$Key_Right & .alpha < 1) {
            # arrow right
            # increase alpha blending
            .alpha <<- 1.1 * .alpha
            datalayer$setOpacity(.alpha)
            qupdate(datalayer)
        }
        else if (key == Qt$Qt$Key_Left & .alpha > 1/n) {
            # arrow left
            # decrease alpha blending
            .alpha <<- 0.9 * .alpha
            datalayer$setOpacity(.alpha)
            qupdate(datalayer)
        } else if (key == Qt$Qt$Key_Z) {
					zoom <<- !zoom
  				print(sprintf("zoom: %s", zoom))
  		  } else if (key == Qt$Qt$Key_X) {
					zoom_focal <<- !zoom_focal
					#browser()
  		#		print(sprintf("focal zoom <%s>", event$pos()))
				} else if (key == Qt$Qt$Key_R) {
					# reset to original boundaries

  				dataRanges <<- c(make_data_ranges(xlim), make_data_ranges(ylim)) 
	
					xaxis$setLimits(qrect(dataRanges[1:2], c(0, 1)))
					yaxis$setLimits(qrect(c(0, 1), dataRanges[3:4]))

					lims <<- qrect(dataRanges[1:2], dataRanges[3:4])

					bglayer$setLimits(lims)
					datalayer$setLimits(lims)
					brushlayer$setLimits(lims)
					querylayer$setLimits(lims)
		
					qupdate(root)

				}


    }
		handle_zoom <- function() {
		print("zoom action")
		print(.zstart)
		print(.zstop)		
			if (!all(.zstart == .zstop))
				dataRanges <<- c(min(.zstart[1],.zstop[1]), max(.zstart[1],.zstop[1]), 
								 min(.zstart[2],.zstop[2]), max(.zstart[2],.zstop[2])) 
			else # reset zoom to default
				dataRanges <<- c(make_data_ranges(xlim), make_data_ranges(ylim)) 
	
			.zstart <<- .zstop <<- NULL
			
			xaxis$setLimits(qrect(dataRanges[1:2], c(0, 1)))
			yaxis$setLimits(qrect(c(0, 1), dataRanges[3:4]))

			lims <<- qrect(dataRanges[1:2], dataRanges[3:4])

			bglayer$setLimits(lims)
			datalayer$setLimits(lims)
			brushlayer$setLimits(lims)
			querylayer$setLimits(lims)

			qupdate(root)
		}

		handle_focal_zoom <- function(focus, speed,  forward) {
	
			xscale <- (dataRanges[2]-focus[1])/(focus[1] - dataRanges[1])
			yscale <- (dataRanges[4]-focus[2])/(focus[2] - dataRanges[3])
			sgn <- if (forward) 1 else -1
		
			newRanges <- dataRanges
			newRanges[1] <- dataRanges[1] + sgn* speed*diff(dataRanges[1:2])
			newRanges[2] <- focus[1] + (focus[1]-newRanges[1]) * xscale
			newRanges[3] <- dataRanges[3] + sgn*speed*diff(dataRanges[3:4])
			newRanges[4] <- focus[2] + (focus[2]-newRanges[3]) * yscale

			df <- data.frame(data)
			sub <- subset(df, (x >= newRanges[1]) & 
													(x <= newRanges[2]) & 
													(y >= newRanges[3]) & 
													(y <= newRanges[4])   )

			values_out_of_plotting_range <<- nrow(sub) < nrow(df)	
			if (nrow(sub) >= 2) {
				dataRanges <<- newRanges 
				
				xaxis$setLimits(qrect(dataRanges[1:2], c(0, 1)))
				yaxis$setLimits(qrect(c(0, 1), dataRanges[3:4]))
	
				lims <<- qrect(dataRanges[1:2], dataRanges[3:4])
	
				bglayer$setLimits(lims)
				datalayer$setLimits(lims)
				brushlayer$setLimits(lims)
				querylayer$setLimits(lims)
	
				qupdate(root)
			}
		}

    ## record the coordinates of the mouse on click
    brush_mouse_press <- function(item, event) {
				if (zoom_focal) {
					handle_focal_zoom(as.numeric(event$pos()), 2, event$modifiers() != Qt$Qt$ShiftModifier )
				} else { 
					if (zoom) {
					.zstart <<- as.numeric(event$pos())
				} else {
					.bstart <<- as.numeric(event$pos())
					## on right click, we can resize the brush; left click: only move the
					## brush
					if (event$button() == Qt$Qt$RightButton) {
							.bmove <<- FALSE
					}
					if (event$button() == Qt$Qt$LeftButton) {
							.bmove <<- TRUE
					}
				}
			}
    }

    mouse_release <- function(item, event) {
#print(zoom)
				if (zoom) {
					.zstop <<- as.numeric(event$pos())
					handle_zoom()
				} 
		}

    identify_mouse_move <- function(layer, event) {
			if (zoom) {
#print("identify_mouse_move")
				.zstop <<- as.numeric(event$pos())
				qupdate(querylayer)
				return()
			}
        pos <- event$pos()
        .bpos <<- as.numeric(pos)
        ## simple click: don't change .brange
        if (!all(.bpos == .bstart) && (!.bmove)) {
            .brange <<- .bpos - .bstart
        }
        .new.brushed <- rep(FALSE, n)
        xrange <- .radius/root$size$width() * diff(dataRanges[c(1, 2)])
        yrange <- .radius/root$size$height() * diff(dataRanges[c(3, 4)])
        #
        rect <- qrect(matrix(c(.bpos - .brange - c(xrange, yrange),
                               .bpos + .brange + c(xrange, yrange)),
                             2, byrow = TRUE))
        ##browser()
        ##rect = qrect(matrix(c(.bpos - .brange, .bpos + .brange), 2,
        ##             byrow = TRUE))
        hits <- layer$locate(rect) + 1

        .new.brushed[hits] <- TRUE
        data$.brushed <- mode_selection(data$.brushed, .new.brushed,
                                        mode = brush(data)$mode)
    }

    handle_wheel_event <- function(layer, event) {
#			print("wheeling")
#			browser()
			handle_focal_zoom(as.numeric(event$pos()), event$delta()/200.0,  TRUE)
    }

    # Display category information on hover (query) ----------------------------
    .queryPos <- NULL

    query_draw <- function(item, painter, exposed, ...) {
				if (zoom) {
					if (!is.null(.zstart)) {
						if (!all(.zstart == .zstop)) {
							# draw zoom rectangle
				rect <- qrect(min(.zstart[1],.zstop[1]), max(.zstart[1],.zstop[1]), 
								 min(.zstart[2],.zstop[2]), max(.zstart[2],.zstop[2]))
				qdrawRect(painter, min(.zstart[1],.zstop[1]),  
								 min(.zstart[2],.zstop[2]), max(.zstart[1],.zstop[1]), max(.zstart[2],.zstop[2]),
								 stroke="black", fill=rgb(.1,.1,.1, alpha=0.5))
				return()
						}
					}
				}

        if (is.null(.queryPos))
            return()

        xpos <- .queryPos[1]
        ypos <- .queryPos[2]

        xrange <- .radius/root$size$width() * diff(dataRanges[c(1, 2)])
        yrange <- .radius/root$size$height() * diff(dataRanges[c(3, 4)])
        rect <- qrect(matrix(c(xpos - xrange, ypos - yrange,
                               xpos + xrange, ypos + yrange),
                             2, byrow = TRUE))
        hits <- datalayer$locate(rect) + 1
        #print(hits)
        #print(.queryPos)
        #browser()
        # Nothing under mouse?
        if (length(hits) == 0)
            return()

        info <- data.frame(x[hits], y[hits])
        names(info) <- c(xlab, ylab)
        #browser()

        # Nothing under mouse
        #    if (nrow(info) == 0) return()

				d <- options()$str$digits.d
        # Work out label text
        idx <- names(info)
        if (length(hits) == 1) {
            infodata <- as.character(round(unlist(info[, idx]), d))
            infostring <- paste(idx, infodata, collapse = "\n", sep = ": ")
        }
        else {
            #browser()
            xymin <- unlist(lapply(info[, idx], min, na.rm = TRUE))
            xymax <- unlist(lapply(info[, idx], max, na.rm = TRUE))
					  xymin <- round(xymin, d)
					  xymax <- round(xymax, d)
            infostring <- paste(idx, paste(xymin, xymax, sep = " - "),
                                collapse = "\n", sep = ": ")
            infostring <- paste(sprintf("%s points\n", length(hits)), infostring)
        }
				infostring <- paste("\n", infostring)
        bgwidth <- qstrWidth(painter, infostring)
        bgheight <- qstrHeight(painter, infostring)

        ## adjust drawing directions when close to the boundary
        hflag <- dataRanges[2] - xpos > bgwidth
        vflag <- ypos - dataRanges[3] > bgheight
        qdrawRect(painter, xpos, ypos, xpos + ifelse(hflag, 1, -1) * bgwidth,
                  ypos + ifelse(vflag, -1, 1) * bgheight,
                  stroke = rgb(1, 1, 1), fill = rgb(1, 1, 1, 0.9))

        qstrokeColor(painter) <- brush(data)$label.color
        qdrawText(painter, infostring, xpos, ypos,
                  halign = ifelse(hflag, "left", "right"),
                  valign = ifelse(vflag, "top", "bottom"))

    }

    query_hover <- function(item, event, ...) {
        #    if (.brush) return()

        .queryPos <<- as.numeric(event$pos())
        qupdate(querylayer)
    }

    query_hover_leave <- function(item, event, ...) {
        .queryPos <<- NULL
        qupdate(querylayer)
    }

    ########## end event handlers

    ###################
    # draw the canvas #
    ###################
    xWidth <- 600
    yWidth <- 600
    if (!is.null(aspect.ratio))
        yWidth <- round(1.0 * xWidth * aspect.ratio, 0)

#print(yWidth)

#    size <- qsize(as.integer(c(xWidth, yWidth)))
#    limits <- qrect(c(0, 1), c(0, 1))
    scene <- qscene()
    root <- qlayer(scene, cache=cache)
#    root$setGeometry(qrect(0, 0, xWidth, yWidth))

		xaxis <- qlayer(parent=root, paintFun = xaxis, limits = qrect(dataRanges[1:2], c(0, 1)),
                      cache=cache, row=2, col=1, clip=FALSE)
		yaxis <- qlayer(parent=root, paintFun = yaxis, limits = qrect(c(0, 1), dataRanges[3:4]),
                      cache=cache, row=1, col=0, clip=FALSE)

# clipping needs to be turned off - otherwise small integer values cause strange results
    bglayer <- qlayer(parent= root, paintFun = grid, limits = lims, cache=cache, row=1, col=1, clip=FALSE)

    datalayer <- qlayer(parent = root, paintFun = scatter.all,
                        keyPressFun = keyPressFun,
                        mouseMoveFun = identify_mouse_move,
                        mousePressFun = brush_mouse_press,
                        mouseReleaseFun = mouse_release,
												focusInFun = function(...) {
													focused(data) <- TRUE
												},
												focusOutFun = function(...) {
													focused(data) <- FALSE
												},
												wheelFun = handle_wheel_event,
                        limits = lims, cache=cache, row=1, col=1, clip=FALSE)
    brushlayer <- qlayer(parent = root, paintFun = brush_draw, limits = lims,
                         cache=cache, row=1, col=1, clip=FALSE)
    querylayer <- qlayer(parent = root, query_draw, limits = lims,
                         hoverMoveFun = query_hover,
                         hoverLeaveFun = query_hover_leave, cache=cache, row=1, col=1, clip=FALSE)
 
    layout = root$gridLayout() 
    layout$setRowPreferredHeight(0, 40)	# width of yaxis
    ## the y-axis layer needs 'dynamic' width determined by #{characters}
    ## here is a formula by my rule of thumb: 9 * nchar + 5
    layout$setColumnPreferredWidth(0, 50)
    layout$setRowPreferredHeight(2, 50)
    layout$setColumnMaximumWidth(2, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setColumnStretchFactor(0, 0)
    layout$setRowStretchFactor(2, 0)

 # space on right side
    layout$setColumnPreferredWidth(3,40) # height of xaxis
    layout$setColumnStretchFactor(3, 0)
    

 		view <- qplotView(scene = scene)

    title <- sprintf("Scatterplot of %s and %s", xlab, ylab)
    view$setWindowTitle(title)
    # view$setMaximumSize(plot1$size)

    ######################
    # add some listeners #
    ######################
    #if (is.mutaframe(data)) {
    func <- function(i, j) {
    	switch(j, .brushed = qupdate(brushlayer),
            .color = qupdate(datalayer), 
            {	# any other event
                datalayer$invalidateIndex()
                qupdate(datalayer)
								qupdate(brushlayer)
            })
    }

    add_listener(data, func)
    #}
    view$resize(xWidth, yWidth)




    return(view)

}
