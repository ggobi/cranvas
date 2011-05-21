
##' Interactive Maps.
##' Create an interactive map from qdata
##'
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
    
    
    windowRanges <- make_window_ranges(dataRanges, "", "", ytickmarks = "", main = .df.title)
    
    lims <- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])
    
    
    draw <- function(item, painter, exposed) {        
			
        for (j in 1:nrow(groupdata)) {
        		i <- groupdata$group[j]
            xx <- x[group == j]
            yy <- y[group == j]
            qdrawPolygon(painter, xx, yy, stroke = "grey80", fill = groupdata$color[j])
        }
        
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
				if (groupdata$.brushed[i]) {
					xx <- x[group == j]
					yy <- y[group == j]
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
				if (length(hits) > 0) 
	        groupdata[hits,]$.brushed <<- TRUE

    }
    
    setSelected <- function() {
			bdata <- subset(groupdata, .brushed == TRUE)
			brushed <- group %in% bdata$group

#			.new.brushed[hits] = TRUE
#			focused(data) <- TRUE
			selected(data) = mode_selection(selected(data), brushed, mode = brush(data)$mode)			
	  }
    
    
    # Key board events ---------------------------------------------------------
    
    keyPressFun <- function(item, event, ...) {
        if (event$key() == Qt$Qt$Key_Shift) 
            .extended <<- !.extended
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
        rect = qrect(matrix(c(xpos, ypos, xpos + 1e-04, ypos + 1e-04), 2, byrow = TRUE))
        hits <- datalayer$locate(rect) + 1
        
        infostring = paste(sprintf("\n %s: ", labelID),  groupdata[hits,]$label, collapse="\n")
        
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
        if (is.null(arguments$colour)) 
            return()
        
        xpos = max(x)
        ypos = (max(y) + min(y))/2
        #browser()
        qstrokeColor(painter) <- "black"
        qdrawText(painter, deparse(arguments$colour), xpos, ypos, valign = "top", 
            halign = "left")
        fontHeight <- qstrHeight(painter, deparse(arguments$colour))
        
        # create a set of rectangles
        r0 <- 0.05 * c(0, 0, diff(range(x)), diff(range(y)))
        ypos <- ypos - 3 * qstrHeight(painter, deparse(arguments$colour))
        r0 <- r0 + c(xpos, ypos)
        
        col <- eval(arguments$colour, .groupsdata)
        d <- options()$str$digits.d
        #   browser()
        qcol <- round(quantile(col, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T, 
            names = FALSE), d)
        
        for (i in 1:length(qcol)) {
            qdrawRect(painter, r0[1], r0[2], r0[3], r0[4], fill = scale_color(col, 
                qcol[i]), stroke = "black")
            qdrawText(painter, as.character(qcol[i]), xpos + 1.5 * (r0[3] - r0[1]), 
                ypos + fontHeight, valign = "top", halign = "left")
            ypos <- ypos - 1.5 * fontHeight
            r0[2] <- r0[2] - 1.5 * fontHeight
            r0[4] <- r0[4] - 1.5 * fontHeight
        }
        
    }
    
    scene = qscene()
    
    bglayer = qlayer(scene, coords, limits = lims, clip = FALSE)
    datalayer = qlayer(scene, draw, limits = lims, 
				focusOutFun = function(layer, painter) {
	print("focus off map")
            focused(data) = FALSE
        },
				focusInFun = function(layer, painter) {
	print("focus on map")
            focused(data) = TRUE
        },
        clip = FALSE)
    brushing_layer = qlayer(scene, brushing_draw, mousePressFun = brushing_mouse_press, 
        mouseMoveFun = brushing_mouse_move, mouseReleaseFun = brushing_mouse_release, 
        keyPressFun = keyPressFun, limits = lims, clip = FALSE)
    querylayer = qlayer(scene, query_draw, hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave, 
        limits = lims, clip = FALSE)
    legendlayer = qlayer(scene, legend_draw, limits = lims, clip = FALSE)
    
    
    
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

      
    qplotView(scene = scene)
}
 
