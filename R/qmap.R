##' Add Colour to Interactive Maps
##'
##' Creates a choropleth map from an interactive map generated with qmap.
##'
##' 'C' computes a cartogram representation 
##' @param qmap a mutaframe for the ap representation
##' @param qdata is the mutaframe containing the variable to be displayed as colour
##' @param label is the variable in the qdata mutaframe to be displayed
##' @param scale is a color scheme from the scales package, options include scale_colour_gradient(), scale_colour_gradient2(), ...
##' @author Heike Hofmann
##' @export
##' @example inst/examples/qmap-ex.R

setMapColorByLabel <- function(qmap, qdata, label, scale = scale_colour_gradient()) {
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
	 link <- as.character(unique(qdata[,link_var(qdata)]))
   scale$coldf <- data.frame(values=label, regions=link)
	 lcolor <- rep(NA, nrow(qmap))
	 for (i in 1:length(link)) {
			j <- which(qmap[,link_var(qmap)] == link[i])
			lcolor[j] <- colors[i]
	 }
	 qmap$.color <- lcolor
	 attr(qmap, "col.scale") <- scale
}

##' Interactive Maps
##'
##' Create an interactive map from qdata.
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
##' @example inst/examples/qmap-ex.R
qmap <- function(data, longitude, latitude, group, label = group,
    main = NULL, filled = TRUE, ...) {

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

    ## cartogram changed?
    cartogram_this <- FALSE
    at_map <- TRUE
    cartmaxstep <- 20
    cartstep <- 0

    if (is.null(main))
        .df.title <- paste("Map of", deparse(substitute(data)))

    dataRanges <- c(extend_ranges(x), extend_ranges(y))

    # space in window around plot (margins in base R)
    # this space depends on the labels needed on the left
    # find out about these first:

    lims <- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])

    draw <- function(item, painter, exposed) {
      if (filled) {
        for (j in 1:nrow(groupdata)) {
        		i <- groupdata$group[j]
            xx <- x[group == i]
            yy <- y[group == i]
            qdrawPolygon(painter, xx, yy, stroke = "grey80", fill = groupdata$color[j])
        }
 #print(table(groupdata$color))
#	print("draw legend")
      } else {
        for (j in 1:nrow(groupdata)) {
        		i <- groupdata$group[j]
            xx <- x[group == i]
            yy <- y[group == i]
            qdrawPolygon(painter, xx, yy, stroke = groupdata$color[j], fill = NULL)
        }
      }

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

      # helper functions
      df2map <- function(mapdf) {
        res <- ddply(mapdf, .(group), summarize, 
          x = c(long, NA),
          y = c(lat, NA)
        )
        x <- res$x
        y <- res$y
        chr <- ddply(mapdf, .(group), summarize, 
          names = region[1]
        )
        
        return(list(x=x, y=y, 
                    range=c(range(mapdf$long), range(mapdf$lat)),
                    names=as.character(chr$names)))
      }

      map2df <- function(map) {
        pls <- slot(map, "polygons")
        n <- length(pls)
      
        res <- ldply(pls, function(x) {
          pl <- slot(x, "Polygons")
          id <- slot(x, "ID")
              m <- length(pl)
          coords <- data.frame()
              for (j in 1:m) {
                crds <- data.frame(slot(pl[[j]], "coords"))
                  names(crds) <- c("long", "lat")
                  crds$ID <- id
            coords <- rbind(coords, crds)
               }
          return(coords)
        })
        return(res)
      }

    keyPressFun <- function(item, event, ...) {
        if (event$key() == Qt$Qt$Key_Shift) {
            .extended <<- !.extended
        print("extended:")
        print(.extended)
}
        if (event$key() == Qt$Qt$Key_R) {
          dataRanges <<- c(extend_ranges(x), extend_ranges(y))

          lims <<- qrect(dataRanges[1:2], dataRanges[3:4])

          bglayer$setLimits(lims)
          datalayer$setLimits(lims)
          brushing_layer$setLimits(lims)
          querylayer$setLimits(lims)

          qupdate(root_layer)

        } 
        if (event$key() == Qt$Qt$Key_F) {
          filled <<- !filled
          qupdate(root_layer)         
        }
        if (event$key() == Qt$Qt$Key_C) {
          print("calculate cartogram:\n")
          if (!is.null(attr(data, "col.scale"))) {
            # need to check that it is a continuous color scheme
            if (is.numeric(attr(data, "col.scale")$coldf[,"values"])) {

## create choropleth map            
              # first example was from usacrimes
              usacrimes <- attr(data, "col.scale")$coldf
              usacrimes <- unique(merge(usacrimes, data.frame(label=groupdata$label), by.x="regions", by.y="label", all.y=T))
              idx <- which(is.na(usacrimes$values))
              ## set missing values to ten percent below minimum, unless negative
              usacrimes$values[idx] <- max(min(usacrimes$values, na.rm=T)/2, min(usacrimes$values, na.rm=T) - 0.1*diff(range(usacrimes$values, na.rm=T)))
              row.names(usacrimes) <- as.character(usacrimes$regions)
              usacrimes <- usacrimes[,c(2,1)]

              stmap <- df2map(df.data)
              sp <- maptools::map2SpatialPolygons(stmap, stmap$names)
              spdf <- sp::SpatialPolygonsDataFrame(sp, usacrimes, match.ID = TRUE)
              cart <- cart::cartogram(spdf, variable="values")

              cartpls <- sp::SpatialPolygonsDataFrame(cart, usacrimes, match.ID = TRUE)
              cartdf <- map2df(cartpls)

              cartdf$x <- states$long
              cartdf$y <- states$lat
              cartdf <- transform(cartdf, 
                cartx = (long-min(long))/diff(range(long))*diff(range(x))+min(x),
                carty = (lat-min(lat))/diff(range(lat))*diff(range(y))+min(y)
              )
## first time the data is set, the listeners don't know what to do with them ...
              if (! ("cartx" %in% names(data)) | !("carty" %in% names(data))) {
                data$cartx <- cartdf$cartx
                data$carty <- cartdf$carty
              }
## display choropleth map
              cartogram_this <<- TRUE
              data$cartx <- cartdf$cartx
              data$carty <- cartdf$carty

                            
            }
          }          
        }
        if (event$key() == Qt$Qt$Key_M) {
          at_map <<- TRUE
          cartstep <<- 0
          x <- eval(arguments$longitude, df.data)
          y <- eval(arguments$latitude, df.data)
          qupdate(root_layer)          
        }

        if (event$key() %in% c(Qt$Qt$Key_Right, Qt$Qt$Key_Left)) {  
          if (cartogram_this) {
            if (event$key() == Qt$Qt$Key_Right) {
            # move one step closer from map to cartogram
              if (cartstep < cartmaxstep) {
                cartstep <<- cartstep + 1

                x <- eval(arguments$longitude, df.data)
                y <- eval(arguments$latitude, df.data)
                diffx <- (data$cartx-x)/cartmaxstep
                diffy <- (data$carty-y)/cartmaxstep
                
                x <<- x + cartstep*diffx
                y <<- y + cartstep*diffy
                qupdate(root_layer)
                qupdate(datalayer)
              }
            }
            if (event$key() == Qt$Qt$Key_Left) {
            # move one step closer from cartogram to map
              if (cartstep > 0) {
                cartstep <<- cartstep - 1

                x <- eval(arguments$longitude, df.data)
                y <- eval(arguments$latitude, df.data)
                diffx <- (data$cartx-x)/cartmaxstep
                diffy <- (data$carty-y)/cartmaxstep
                
                x <<- x + cartstep*diffx
                y <<- y + cartstep*diffy
                qupdate(root_layer)
                qupdate(datalayer)
              }
            }          
          }
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
				hflag = dataRanges[2] - xpos > bgwidth
				vflag = ypos - dataRanges[3] > bgheight
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


    scene = qscene()
    root_layer = qlayer(scene)

    bglayer = qlayer(NULL, coords, limits = lims, clip = FALSE)
    datalayer = qlayer(NULL, draw, limits = lims,
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
    legendlayer = qlegend(parent=NULL, data=data, vertical=TRUE)
    brushing_layer = qlayer(NULL, brushing_draw, mousePressFun = brushing_mouse_press,
        mouseMoveFun = brushing_mouse_move, mouseReleaseFun = brushing_mouse_release,
        keyPressFun = keyPressFun, limits = lims, clip = FALSE)
    querylayer = qlayer(NULL, query_draw, hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave,
        limits = lims, clip = FALSE)

  root_layer[1,0] <- bglayer
  root_layer[1,0] <- datalayer
  root_layer[1,0] <- brushing_layer
  root_layer[1,0] <- querylayer
  root_layer[0,0] <- qmtext(side = 3, text = .df.title)
  root_layer[1,1] <- legendlayer

    ## update the brush layer in case of any modifications to the mutaframe
    d.idx = add_listener(data, function(i, j) {
      if (length(j) == 1) {
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
        }, carty={
          # I'm assuming that carty is the second one of the cartogram variables to be changed
          print("got carty event\n")
          if (cartogram_this) {
            print("and will react to it")
            K <- 50
            at_map <<- FALSE
            cartstep <<- cartmaxstep
            diffx <- (x - data$cartx)/K
            diffy <- (y - data$carty)/K
            for (i in 1:K) {
              x <<- x - diffx
              y <<- y - diffy
              qupdate(datalayer)
            }
          }
        }, {
        print(sprintf("uncovered event in map: %s", j))
            df.data <<- data.frame(data)
        
            x <<- eval(arguments$longitude, df.data)
            y <<- eval(arguments$latitude, df.data)
        
            qupdate(root_layer)
            qupdate(datalayer)
            qupdate(brushing_layer)
        })
      } else 
        print(sprintf("qmap::add_listener length(j) == %d", length(j)))      
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
    layout$setRowPreferredHeight(0, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setRowPreferredHeight(1, 500)
    layout$setColumnPreferredWidth(1, 500)
# legend area
    layout$setColumnPreferredWidth(1,100)
    layout$setColumnStretchFactor(1, 0)
#		layout$setColumnMinimumWidth(1,1)
#		layout$setColumnMaximumWidth(1,200)
		qplotView(scene = scene)
}

