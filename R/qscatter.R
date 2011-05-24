#' Draw a scatterplot
#'
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
    
    #############################
    # internal helper functions #
    #############################

    ############################# end internal helper functions

    ################################
    # data processing & parameters #
    ################################

    arguments <- as.list(match.call()[-1])

    ## transform the data
    df <- data.frame(data)
    x <- eval(arguments$x, df)
    y <- eval(arguments$y, df)

		if (is.null(xlim)) xlim <- range(x, na.rm=T)
		if (is.null(ylim)) ylim <- range(y, na.rm=T)
		

    ## parameters for dataRanges
    if(is.null(xlab)) xlab <- deparse(arguments$x)
    if(is.null(ylab)) ylab <- deparse(arguments$y)


    ## parameters for all layers
		dataRanges <- c(make_data_ranges(xlim), 
									  make_data_ranges(ylim))
    windowRanges <- make_window_ranges(dataRanges, xlab, ylab,
                                           ytickmarks = labeled,
                                           xtickmarks = labeled, main = main)

    lims <- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])

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
    .brange <- c(diff(windowRanges[c(1, 2)]), diff(windowRanges[c(3, 4)]))/30

    n <- nrow(data)

    ################################ end data processing & parameters

    ##########
    # layers #
    ##########
    coords <- function(item, painter, exposed) {
				sx <- .axis.loc(dataRanges[1:2])
				sy <- .axis.loc(dataRanges[3:4])
				ylabels <- rep("", length(sy))
				xlabels <- rep("", length(sx))
				
				if (labeled) {
					ylabels <- sy 
					xlabels <- sx 
				}
        # grey background with grid lines
        draw_grid_with_positions_fun(painter, dataRanges, sx, sy)

        # labels as appropriate
				draw_x_axes_with_labels_fun(painter, dataRanges, axisLabel = xlabels,
																		labelHoriPos = sx, name = xlab)

        
				draw_y_axes_with_labels_fun(painter, dataRanges, axisLabel = ylabels,
																		labelVertPos = sy, name = ylab)
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
            #  brushlayer$setOpacity(.alpha)
            qupdate(datalayer)
        }
        else if (key == Qt$Qt$Key_Left & .alpha > 1/n) {
            # arrow left
            # decrease alpha blending
            .alpha <<- 0.9 * .alpha
            datalayer$setOpacity(.alpha)
            #  brushlayer$setOpacity(.alpha)
            qupdate(datalayer)
        }


    }

    ## record the coordinates of the mouse on click
    brush_mouse_press <- function(item, event) {
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

    identify_mouse_move <- function(layer, event) {
        pos <- event$pos()
        .bpos <<- as.numeric(pos)
        ## simple click: don't change .brange
        if (!all(.bpos == .bstart) && (!.bmove)) {
            .brange <<- .bpos - .bstart
        }
        .new.brushed <- rep(FALSE, n)
        xrange <- .radius/root$size$width() * diff(windowRanges[c(1, 2)])
        yrange <- .radius/root$size$height() * diff(windowRanges[c(3, 4)])
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

    # Display category information on hover (query) ----------------------------
    .queryPos <- NULL

    query_draw <- function(item, painter, exposed, ...) {
        if (is.null(.queryPos))
            return()

        xpos <- .queryPos[1]
        ypos <- .queryPos[2]

        xrange <- .radius/root$size$width() * diff(windowRanges[c(1, 2)])
        yrange <- .radius/root$size$height() * diff(windowRanges[c(3, 4)])
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

        # Work out label text
        idx <- names(info)
        if (length(hits) == 1) {
            infodata <- as.character(unlist(info[, idx]))
            infostring <- paste(idx, infodata, collapse = "\n", sep = ": ")
        }
        else {
            #browser()
            xymin <- unlist(lapply(info[, idx], min, na.rm = TRUE))
            xymax <- unlist(lapply(info[, idx], max, na.rm = TRUE))
            infostring <- paste(idx, paste(xymin, xymax, sep = " - "),
                                collapse = "\n", sep = ": ")
            infostring <- paste(sprintf("%s points\n", length(hits)), infostring)
        }
				infostring <- paste("\n", infostring)
        bgwidth <- qstrWidth(painter, infostring)
        bgheight <- qstrHeight(painter, infostring)

        ## adjust drawing directions when close to the boundary
        hflag <- windowRanges[2] - xpos > bgwidth
        vflag <- ypos - windowRanges[3] > bgheight
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
    limits <- qrect(c(0, 1), c(0, 1))
    scene <- qscene()
    root <- qlayer(scene, cache=cache)
    root$setGeometry(qrect(0, 0, xWidth, yWidth))
    bglayer <- qlayer(parent = root, paintFun = coords, limits = lims,
                      cache=cache)
    datalayer <- qlayer(parent = root, paintFun = scatter.all,
                        keyPressFun = keyPressFun,
                        mouseMove = identify_mouse_move,
                        mousePressFun = brush_mouse_press,
                        mouseReleaseFun = identify_mouse_move,
												focusInFun = function(...) {
													focused(data) <- TRUE
												},
												focusOutFun = function(...) {
													focused(data) <- FALSE
												},
                        limits = lims, cache=cache)
    brushlayer <- qlayer(parent = root, paintFun = brush_draw, limits = lims,
                         cache=cache)
    querylayer <- qlayer(parent = root, query_draw, limits = lims,
                         hoverMoveFun = query_hover,
                         hoverLeaveFun = query_hover_leave, cache=cache)
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
