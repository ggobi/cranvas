
setMapColorByLabel <- function(qmap, qdata, label, scale) {
	if (is.null(link_var(qmap)) | is.null(link_var(qdata)))
		error("data sets need to be linked")

   arguments <- as.list(match.call()[-1])
   df.data <- data.frame(qdata)
#
   label <- eval(arguments$label, df.data)
	# defaults, should be overwritten, if specified
	 if (is.null(scale$limits))
		 scale$limits <- range(label)
	 if (is.null(scale$name))
		 scale$name <- deparse(arguments$label)

	 colors <- scale$map(label)
	 link <- unique(qdata[,link_var(qdata)])
	 lcolor <- rep(NA, nrow(qmap))
	 for (i in 1:length(link)) {
			j <- which(qmap[,link_var(qmap)] == link[i])
			lcolor[j] <- colors[i]
	 }
	 qmap$.color <- lcolor
	 attr(qmap, "col.scale") <- scale
}

##' Interactive Maps.
##' Create an interactive map from qdata
##'
##' mouse wheel events are translated to focal zoom
##'
##' 'R' resets maps to original scale after zoom
##' @param data a mutaframe which is typically built upon a data frame
##' along with several row attributes
##' @param latitude spatial x variable
##' @param longitude spatial y variable
##' @param group grouping variable for polygons
##' @param main window title, by default 'Map of <data set>'
##' @param ...
##' @return NULL
##' @author Heike Hofmann
##' @export
##' @example cranvas/inst/examples/maps-ex.R
qmap <- function(data, longitude, latitude, group, label = group,
    main = NULL, ...) {

    ## check if an attribute exist
    #  browser()
    arguments <- as.list(match.call()[-1])
    df.data <- data.frame(data)

    x <- eval(arguments$longitude, df.data)
    y <- eval(arguments$latitude, df.data)

    group <- df.data$group <- eval(arguments$group, df.data)
    df.data$label <- eval(arguments$label, df.data)

		## aggregate data on group level
		groupdata <- ddply(df.data, .(group), summarize,
			color = as.character(.color)[1],
			label = as.character(label)[1],
			.brushed = FALSE
		)
		labelID <- deparse(arguments$label)

    ## parameters for the brush
    .brush.attr = attr(data, ".brush.attr")

    if (is.null(main))
        .df.title <- paste("Map of", deparse(substitute(data)))

    dataRanges <- c(make_data_ranges(c(min(x), max(x))), make_data_ranges(range(y)))

    # space in window around plot (margins in base R)
    # this space depends on the labels needed on the left
    # find out about these first:


    windowRanges <- make_window_ranges(dataRanges, "", "", ytickmarks = FALSE, main = .df.title)

    lims <- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])


    draw <- function(item, painter, exposed) {

        for (j in 1:nrow(groupdata)) {
        		i <- groupdata$group[j]
            xx <- x[group == i]
            yy <- y[group == i]
            qdrawPolygon(painter, xx, yy, stroke = "grey80", fill = groupdata$color[j])
        }
 #print(table(groupdata$color))
#	print("draw legend")


 				add_title_fun(painter, dataRanges, title = .df.title)
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

        qdrawRect(painter, left, bottom, right, top, fill = rgb(0, 0, 0, alpha = 0.3),
            stroke = "black")
    }

    recalcbrushed <- function() {
			for (i in 1:nrow(groupdata)) {
					brushed <- data$.brushed[group == groupdata$group[i]]
					groupdata$.brushed[i] <<- any(brushed)
			}
    }


    brushing_draw <- function(item, painter, exposed, ...) {
			brushcolor <- brush(data)$color

			for (j in 1:nrow(groupdata)) {
				i <- groupdata$group[j]
				if (groupdata$.brushed[j]) {
					xx <- x[group == i]
					yy <- y[group == i]
					qdrawPolygon(painter, xx, yy, stroke = "grey80", fill = brushcolor)
				}
			}

			if (!is.null(.endBrush)) {
					drawBrush(item, painter, exposed)
			}
    }

    brushing_mouse_press <- function(item, event, ...) {
        #print('brushing_mouse_press')
        .brush <<- TRUE
        if (is.null(.startBrush)) {
            .startBrush <<- as.numeric(event$pos())
            .endBrush <<- as.numeric(event$pos())
        }

        setHiliting()
        qupdate(brushing_layer)
    }

    brushing_mouse_move <- function(item, event, ...) {
        #print('brushing_mouse_move')
        .endBrush <<- as.numeric(event$pos())

        setHiliting()
        qupdate(brushing_layer)
    }

    brushing_mouse_release <- function(item, event, ...) {
        #print('brushing_mouse_release')
        .endBrush <<- as.numeric(event$pos())
        setHiliting()
        qupdate(brushing_layer)


        .brush <<- FALSE


        .startBrush <<- NULL
        .endBrush <<- NULL
				focused(data) <- TRUE
        setSelected()
				focused(data) <- FALSE
    }

    setHiliting <- function() {
        left = min(.startBrush[1], .endBrush[1])
        right = max(.startBrush[1], .endBrush[1]) + 1e-08
        top = max(.startBrush[2], .endBrush[2]) + 1e-08
        bottom = min(.startBrush[2], .endBrush[2])

        rect = qrect(matrix(c(left, bottom, right, top), 2, byrow = TRUE))
        hits <- datalayer$locate(rect) + 1

        groupdata$.brushed <<- FALSE
				if (length(hits) > 0) {
				# link by label variable
					idx <- which(groupdata$label %in% groupdata[hits,]$label)
	        groupdata[idx,]$.brushed <<- TRUE
				}
    }

    setSelected <- function() {
			bdata <- subset(groupdata, .brushed == TRUE)
			brushed <- group %in% bdata$group

#			.new.brushed[hits] = TRUE
#			focused(data) <- TRUE
			selected(data) = mode_selection(selected(data), brushed, mode = brush(data)$mode)
	  }
    # Wheel events -------------------------------------------------------------



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

				lims <<- qrect(dataRanges[1:2], dataRanges[3:4])

				bglayer$setLimits(lims)
				datalayer$setLimits(lims)
				brushing_layer$setLimits(lims)
				querylayer$setLimits(lims)

				qupdate(root_layer)
			}
		}


    handle_wheel_event <- function(layer, event) {
#			print("wheeling")
#			browser()
			handle_focal_zoom(as.numeric(event$pos()), event$delta()/200.0,  TRUE)
    }


    # Key board events ---------------------------------------------------------

    keyPressFun <- function(item, event, ...) {
        if (event$key() == Qt$Qt$Key_Shift) {
            .extended <<- !.extended
        print("extended:")
        print(.extended)
}
        if (event$key() == Qt$Qt$Key_R) {
          dataRanges <<- c(make_data_ranges(c(min(x), max(x))), make_data_ranges(range(y)))

          lims <<- qrect(dataRanges[1:2], dataRanges[3:4])

          bglayer$setLimits(lims)
          datalayer$setLimits(lims)
          brushing_layer$setLimits(lims)
          querylayer$setLimits(lims)

          qupdate(root_layer)

        }
    }


    # Display category information on hover (query) ----------------------------
    .queryPos <- NULL

    query_draw <- function(item, painter, exposed, ...) {
			# Don't draw when brushing
			if (.brush) return()
			if (is.null(.queryPos)) return()

			xpos <- .queryPos[1]
			ypos <- .queryPos[2]
			rect = qrect(matrix(c(xpos, ypos, xpos + 1e-04, ypos + 1e-04), 2, byrow = TRUE))
			hits <- datalayer$locate(rect) + 1

			infostring = paste(sprintf("\n %s: ", labelID),  groupdata[hits,]$label, collapse="\n")

			if (length(hits) > 0) {
				brushcolor <- brush(data)$color
				for (j in hits) {
					i <- groupdata$group[j]
					xx <- x[group == j]
					yy <- y[group == j]
					qdrawPolygon(painter, xx, yy, stroke = brushcolor)
				}

				bgwidth = qstrWidth(painter, infostring)
				bgheight = qstrHeight(painter, infostring)

				## adjust drawing directions when close to the boundary
				hflag = windowRanges[2] - xpos > bgwidth
				vflag = ypos - windowRanges[3] > bgheight
				qdrawRect(painter, xpos, ypos, xpos + ifelse(hflag, 1, -1) * bgwidth, ypos +
						ifelse(vflag, -1, 1) * bgheight, stroke = rgb(1, 1, 1, 0.95), fill = rgb(1,
						1, 1, 0.95))

				qstrokeColor(painter) = brush(data)$label.color
				qdrawText(painter, infostring, xpos, ypos, halign = ifelse(hflag, "left",
						"right"), valign = ifelse(vflag, "top", "bottom"))
			}
    }

    query_hover <- function(item, event, ...) {
        if (.brush)
            return()

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
        #print('legend_draw')
#browser()
        if (is.null(attr(data,"col.scale"))) {
					layout$setColumnPreferredWidth(1,0)
					return()
        }
				layout$setColumnPreferredWidth(1,200)

#				print("draw legend")

        scale <- attr(data,"col.scale")
#        xpos = max(x)
				xpos = min(x)
        ypos = (max(y) + min(y))/2
        #browser()
        qstrokeColor(painter) <- "black"
        qdrawText(painter, scale$name, xpos, ypos, valign = "top",
            halign = "left")
        fontHeight <- qstrHeight(painter, scale$name)

        # create a set of rectangles
        r0 <- 0.05 * c(0, 0, diff(range(x)), diff(range(y)))
        ypos <- ypos - 3 * qstrHeight(painter, scale$name)
        r0 <- r0 + c(xpos, ypos)

        d <- options()$str$digits.d
        #   browser()
        qval <- seq(scale$limits[1],scale$limits[2],length=5)
        qcol <- scale$map(qval)


        for (i in 1:length(qcol)) {
            qdrawRect(painter, r0[1], r0[2], r0[3], r0[4], fill = qcol[i],
            	stroke = "black")
            qdrawText(painter, as.character(round(qval[i],d)), xpos + 1.5 * (r0[3] - r0[1]),
                ypos + fontHeight, valign = "top", halign = "left")
            ypos <- ypos - 1.5 * fontHeight
            r0[2] <- r0[2] - 1.5 * fontHeight
            r0[4] <- r0[4] - 1.5 * fontHeight
        }

    }

    scene = qscene()
    root_layer = qlayer(scene)

    bglayer = qlayer(root_layer, coords, limits = lims, clip = FALSE)
    datalayer = qlayer(root_layer, draw, limits = lims,
        focusInFun = function(...) {
          print("focus map on")
          focused(data) <- TRUE
        },
        focusOutFun = function(...) {
          print("focus map off")
          focused(data) <- FALSE
        },
        wheelFun = handle_wheel_event,
        clip = FALSE)
    legendlayer = qlayer(scene, legend_draw, limits = lims, clip = FALSE)
    brushing_layer = qlayer(root_layer, brushing_draw, mousePressFun = brushing_mouse_press,
        mouseMoveFun = brushing_mouse_move, mouseReleaseFun = brushing_mouse_release,
        keyPressFun = keyPressFun, limits = lims, clip = FALSE)
    querylayer = qlayer(root_layer, query_draw, hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave,
        limits = lims, clip = FALSE)

  root_layer[0,1] <- legendlayer

    ## update the brush layer in case of any modifications to the mutaframe
    d.idx = add_listener(data, function(i, j) {
			switch(j, .brushed = {
				recalcbrushed()
				qupdate(brushing_layer)
			}, .color = {
				for (i in 1:nrow(groupdata)) {
					cc <- data$.color[group == groupdata$group[i]]
					groupdata$color[i] <<- cc[1]
				}
				# change legend accordingly

				qupdate(datalayer)
				qupdate(brushing_layer)
			}, {
			print(sprintf("uncovered event in map: %s", j))
			})
    })

    ## update the brush layer if brush attributes change
    brush_update = function() {
        qupdate(brushing_layer)
    }
    b.idx = brush(data)$colorChanged$connect(brush_update)
    qconnect(datalayer, 'destroyed', function(x) {
        brush(data)$colorChanged$disconnect(b.idx)
        remove_listener(data, d.idx)
    })



   layout = root_layer$gridLayout()
# map area
    layout$setRowPreferredHeight(0, 400)
    layout$setColumnPreferredWidth(0, 400)
# legend area
    layout$setColumnPreferredWidth(1,100)
    layout$setColumnStretchFactor(1, 0)
#		layout$setColumnMinimumWidth(1,1)
#		layout$setColumnMaximumWidth(1,200)
		qplotView(scene = scene)
}

