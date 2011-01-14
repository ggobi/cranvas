e <- function() {
	
	library(qtbase)
	library(qtpaint)
	library(reshape)
	library(plyr)
	library(plumbr)
	library(stringr)
}


#' Create a hist plot
#' Create a hist plot from 1-D numeric data
#'
#' @param data vector of numeric data to be made into a histogram
#' @param horizontal boolean to decide if the bars are horizontal or vertical
#' @param ... arguments supplied to hist() or the hist layer
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples # torture
#'		rows <- 1000000
#'		bigData <- qmutaframe(data.frame(x = rnorm(rows), y = floor(rnorm(rows) * 7)))
#'		qhist(bigData)
#'
#'		# each column is split evenly
#'		qhist(bigData, splitByCol = "y", title = "Torture - stack")
#'		qhist(bigData, splitByCol = "y", title = "Torture - stack", horizontal = FALSE)
#'
#'		# each column has similar height colors
#'		qhist(bigData, splitByCol = "y", title = "Torture - dodge", position = "dodge")
#'
#'		# range from 0 to 1
#'		qhist(bigData, splitByCol = "y", title = "Torture - relative", position = "relative")
#'
#'
#'  # color tests
#'		# all color is defined
#'		qhist(mtcars, "disp", horizontal = TRUE, fill = "gold", stroke = "red4")
#'
#'		# stacked items
#'		qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "stack", title = "mtcars - stack")
#'
#'		# raw value items
#'		qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "identity", title = "mtcars - identity")
#'
#'		# dodged items
#'		qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "dodge", title = "mtcars - dodge")
#'
#'		# range from 0 to 1
#'		qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "relative", title = "mtcars - relative")
qhist <- function(
	data,
	xCol = 1,
	splitByCol = -1,
	horizontal = TRUE,
	position = "none",
	color = NULL,
	fill = NULL,
	stroke = NULL,
	title = NULL,
	name = names(data),
	ash = FALSE,
	start = min(data[[xCol]]),
	nbins = round(sqrt(nrow(data)),0),
	binwidth = NULL,
	bin_algo_str = NULL,
	...
) {

	# "Global" variables (start with a ".")
	.view <- c()
	.scene <- c()
	.type <- c()
	# .bin_col <- ""
	# .label_col <- ""
	.mf_data_col_names <- c()
	.startBrush <- NULL
	.endBrush <- NULL
	.brush <- FALSE
	.bar_queryPos <- NULL
	.bar_hover_section <- list(top = -1, bottom = 1, right = -1, left = 1)
	.lims <- c()
	.bars_info <- NULL
	.ranges <- c()
	.yMin <- 0
	.yMax <- 0
	.xlab <- ""
	.ylab <- ""
	.histOriginalBreaksAndStart <- list()
	.updateinfo <- FALSE

	# mf_data <- qmutaframe(data)
	if(!is.mutaframe(data)) 	mf_data <- qmutaframe(data)
	else 										mf_data <- data

	# Set up the data
	if (splitByCol == -1) {
		splitByCol <- "qhist_split_column"
		mf_data[[splitByCol]] <- 1
	}
	
	# sets values to FALSE only if brush does not exist yet
	mf_data <- column_coerce(mf_data, ".brushed", FALSE)

	# .bin_col <- data_bin_column(mf_data)
	.mf_data_col_names <- rep("", nrow(mf_data))
	#print(head(mf_data))

	# Set up wrapper functions.
	dataCol <- function() {mf_data[, xCol]}
	xColRange <- function() {dataRange(dataCol())}
	maxBinwidthP <- function() {maxBinwidth(dataCol())}
	baseHistP <- function(...) {baseHist(dataCol(), ...)}
	unitShiftP <- function(){ unitShift(dataCol())}
	maxShiftP <- function() {maxShift(dataCol())}
	xMaxStartPosP <- function() {xMaxStartPos(dataCol())}
	xMinStartPosP <- function() {xMinStartPos(dataCol())}
	xMaxEndPosP <- function() {xMaxEndPos(dataCol())}
	calcBinPosP <- function(start, binwidth) {calcBinPosition(start, binwidth, xColRange()[2], xMaxEndPosP())}
	maxHeightP <- function(){maxHeight(dataCol(), ...)}
	# cat(".xMin: ", xMinStartPosP(), ".xMax: ", xMaxEndPosP(), "\n")
	# cat("maxShift(): ", maxShiftP(), "\n")
	# cat("maxBinwidth(): ", maxBinwidthP(), "\n")

	# Find the maximum height of the counts
	.yMax <- maxHeightP()

	if(!is.null(bin_algo_str)){
		temp_breaks <- baseHistP(breaks = bin_algo_str)$breaks
		.type <- list(type = "hist", binwidth = diff(temp_breaks[1:2]), start = temp_breaks[1])

	} else if(!is.null(binwidth)) {
		.type <- list(type = "hist", binwidth = binwidth, start = start)
		
	} else {
		if(nbins < 1) { stop("please supply a correct bin count") }
		temp_breaks <- baseHistP(breaks = nbins)$breaks
		.type <- list(type = "hist", binwidth = diff(xColRange())/nbins, start = start)
	}
	.histOriginalBreaksAndStart <- list(binwidth = .type$binwidth, start = .type$start)
	# cat("\nOriginal Hist Breaks!\n"); print(.histOriginalBreaksAndStart); 
	# cat("start: ");print(start)
	# cat("bin_algo_str: ");print(bin_algo_str)
	# cat("binwidth: ");print(binwidth)
	# cat("nbins: ");print(nbins)
	

	updateBarsInfo <- function() {
		.bars_info <<- continuous_to_bars(
			data = mf_data[, xCol], splitBy = mf_data[, splitByCol], brushed = mf_data[, ".brushed"],
			typeInfo = .type, position = position, color = color, fill = fill, stroke = stroke, ...)

		.mf_data_col_names <<- rep("", length(.mf_data_col_names))
		for(i in 1:nrow(.bars_info$data)){
			rows <- (.bars_info$data$left[i] < dataCol()) & (.bars_info$data$right[i] >= dataCol())
			# cat(i, " - "); print(rows)
			if(any(rows)) {
				.mf_data_col_names[rows] <<- as.character(.bars_info$data[i, "label"])
			}
		}
		cat("data: \n");print(mf_data[1:8,])
		cat("condensed data: \n");print(.bars_info$data[, c("label", "group", "count", "top", "bottom", ".brushed")])
		cat("unique columns: "); print(unique(.mf_data_col_names))
		
		.updateinfo <<- FALSE
	}
	updateBarsInfo()

	updateRanges <- function() {
		# contains c(x_min, x_max, y_min, y_max)
		if (horizontal) {
			.ranges <<- c(make_data_ranges(c(.yMin, .yMax)), make_data_ranges(c(xMinStartPosP(), xMaxEndPosP())))
		} else {
			.ranges <<- c(make_data_ranges(c(xMinStartPosP(), xMaxEndPosP())), make_data_ranges(c(.yMin, .yMax)))
		}
	}
	updateRanges()



	#######################################################
	# Draw Axes
	coords <- function(item, painter, exposed) {

		# updateBarsInfo()
		# updateRanges()
		# updateLims()

		if (horizontal) {
			.ylab <<- name
			.xlab <<- "count"
		} else {
			.ylab <<- "count"
			.xlab <<- name
		}

		# grey background with grid lines
		if (horizontal) {
			draw_grid_with_positions_fun(painter, .ranges, horiPos = make_pretty_axes(.ranges[1:2], .ranges[1], .ranges[2]))
		} else {
			draw_grid_with_positions_fun(painter, .ranges, vertPos = make_pretty_axes(.ranges[3:4], .ranges[3], .ranges[4]))
		}

		# put labels, if appropriate
		draw_x_axes_fun(painter, .ranges, .xlab)
		draw_y_axes_fun(painter, .ranges, .ylab)

		# title
		if(!is.null(title))
			add_title_fun(painter, .ranges, title)
	}


	#######################################################
	# Draw Bars
	hist.all <- function(item, painter, exposed) {
		if (horizontal) {
			qdrawRect(painter,
				xleft = c(.bars_info$data$bottom), #left
				ybottom = c(.bars_info$data$left), # bottom
				xright = c(.bars_info$data$top), # right
				ytop = c(.bars_info$data$right), # top
				stroke = c(.bars_info$data$stroke),
				fill = c(.bars_info$data$fill)# fill
			)
		} else {
			qdrawRect(painter,
				xleft = c(.bars_info$data$left), #left
				ybottom = c(.bars_info$data$bottom), # bottom
				xright = c(.bars_info$data$right), # right
				ytop = c(.bars_info$data$top), # top
				stroke = c(.bars_info$data$stroke),
				fill = c(.bars_info$data$fill)# fill
			)
		}
	}


	#######################################################
	# Key Functions
	keyPressFun <- function(item, event, ...) {
		if(.brush == TRUE)
			return()

		print(event$key())
		key <- event$key()

		if (key == Qt$Qt$Key_Up) {						# arrow up
			.type$binwidth <<- .type$binwidth * 1.10
			if(.type$binwidth > maxBinwidthP()) .type$binwidth <<- maxBinwidthP()

		} else if (key == Qt$Qt$Key_Down) {		# arrow down
			.type$binwidth <<- .type$binwidth / 1.10

		} else if (key == Qt$Qt$Key_Left) {		# arrow left
			.type$start <<- .type$start - unitShiftP()
			# Make sure the start stays close to home
			if(.type$start < xMinStartPosP()) .type$start <<- xMinStartPosP()

		} else if (key == Qt$Qt$Key_Right) {	# arrow left
			.type$start <<- .type$start + unitShiftP()
			# Make sure the start stays close to home
			if(.type$start > xMaxStartPosP()) .type$start <<- xMaxStartPosP()

		} else if (key == Qt$Qt$Key_A) {			# 'a' or 'A' for 'condition'
			.type$type <<- "ash"
			stop("Ash not implemented")

		} else if (key == Qt$Qt$Key_D) {			# 'd' or 'D' for 'condition'
			.type$type <<- "density"
			stop("Ash not implemented")

		} else if (key == Qt$Qt$Key_O) {			# 'o' or 'O' for 'condition'
			.type$type <<- "dot"
			stop("Ash not implemented")

		} else if (key == Qt$Qt$Key_H) {			# 'h' or 'H' for 'condition'
			.type <- list(type = "hist", binwidth = .histOriginalBreaksAndStart$binwidth, start = .histOriginalBreaksAndStart$start)

		}	else if (key == 82) {			# 'r' or 'R' for 'condition'
				if(identical(.type$type, "hist")) {
					print(.histOriginalBreaksAndStart)
					.type$type <<- "hist"
					.type$start <<- .histOriginalBreaksAndStart$start
					.type$binwidth <<- .histOriginalBreaksAndStart$binwidth
				}

		} else if (key == 87) {
			cat("\n\n\nClosing window!!!! - ", qclose(.view), "\n")
		}




		if (key %in% c(Qt$Qt$Key_Up, Qt$Qt$Key_Down, Qt$Qt$Key_Left, Qt$Qt$Key_Right, 82
			# , Qt$Qt$Key_A, Qt$Qt$Key_D, Qt$Qt$Key_O, Qt$Qt$Key_H
			)) {
			message("updating everything")
			updateBarsInfo()
			 qupdate(.scene)
			# qupdate(datalayer)
			# qupdate(hoverlayer)
			# qupdate(brushing_layer)
		}

	}
	#######################################################
	# Brushing

	draw_brush_rect <- function(item, painter, exposed) {
		cat("draw brush rect\n")
		left = min(.startBrush[1], .endBrush[1])
		right = max(.startBrush[1], .endBrush[1])
		top = max(.startBrush[2], .endBrush[2])
		bottom = min(.startBrush[2], .endBrush[2])

		qdrawRect(painter, left, bottom, right, top, fill=rgb(0,0,0,alpha=0.7), stroke="black")
		cat("draw brush rect - done\n")
	}

	brushing_draw <- function(item, painter, exposed, ...) {
		cat("brushing draw\n")
		if (.updateinfo) updateBarsInfo()
		section <- subset(.bars_info$data, (.brushed > 0))

		if (nrow(section) > 0) {
			#  .brush.attr = attr(odata, '.brush.attr')
			# brushcolor <- .brush.attr[,".brushed.color"]
			brushColor <- brush_attr(mf_data, ".brushed.color")
			
			b <- section$bottom
			t <- (section$top - b) * section$.brushed + b
			
			cat("b: "); print(b)
			cat("t: "); print(t)
			cat("top: "); print(section$top)
			cat("perc: "); print(section$.brushed)
			cat("section:\n"); print(.bars_info$data[, c("label", "group", "count", "top", "bottom", ".brushed")])
			#cat("real data:\n"); print(mf_data[mf_data$disp < 200, c("disp", "cyl", ".brushed")])
			
			brushColor <- rep(brushColor, nrow(section))
			brushColorRGBA <- col2rgb(brushColor, TRUE)
#browser()
			if(brushColorRGBA["alpha",][[1]] > 200) {
				brushColorRGBA["alpha",][[1]] <- 200
				newBrushColor <- rgb(brushColorRGBA["red",][[1]], brushColorRGBA["green",][[1]], brushColorRGBA["blue",][[1]], brushColorRGBA["alpha",][[1]], maxColorValue = 255)
				
				rows <- section$.brushed < 1
				brushColor[rows] <- newBrushColor
			}
			
			
			
			if (horizontal)
				qdrawRect(painter, b, section$right, t, section$left, fill = brushColor, stroke = "black")
			else
				qdrawRect(painter, section$left, b, section$right, t, fill = brushColor, stroke = "black")
		}

		if (!is.null(.endBrush)) {
			draw_brush_rect(item, painter, exposed)
		}
		cat("brushing draw - done\n")
	}

	brushing_mouse_press <- function(item, event, ...) {
		cat("brushing mouse press\n")
		.brush <<- TRUE
		if (is.null(.startBrush)) {
			.startBrush <<- as.numeric(event$pos())
		}
		.endBrush <<- as.numeric(event$pos())

		setHiliting()
		qupdate(brushing_layer)
		cat("brushing mouse press - done\n")
	}

	brushing_mouse_move <- function(item, event, ...) {
		cat("brushing mouse move\n")
		.endBrush <<- as.numeric(event$pos())

		setHiliting()
		qupdate(brushing_layer)
		cat("brushing mouse move - done\n")
	}

	brushing_mouse_release <- function(item, event, ...) {
		cat("brushing mouse release\n")
		.endBrush <<- as.numeric(event$pos())
		setHiliting()
		qupdate(brushing_layer)


		.brush <<- FALSE
		.startBrush <<- NULL
		.endBrush <<- NULL

		setSelected()
		cat("brushing mouse release - done\n")
	}

	setHiliting <- function() {
		cat("setHiliting\n")

		leftMouse = min(.startBrush[1], .endBrush[1])
		rightMouse = max(.startBrush[1], .endBrush[1])
		topMouse = max(.startBrush[2], .endBrush[2])
		bottomMouse = min(.startBrush[2], .endBrush[2])

		if (horizontal) {
			leftMouse = min(.startBrush[2], .endBrush[2])
			rightMouse = max(.startBrush[2], .endBrush[2])
			topMouse = max(.startBrush[1], .endBrush[1])
			bottomMouse = min(.startBrush[1], .endBrush[1])
		}

		# rows <<- (.bars_info$data$left <= right) & (.bars_info$data$right >= left) &
		# 	(.bars_info$data$bottom <= top) & (.bars_info$data$top >= bottom)
		# .bars_info$data$.brushed <<- rows

		valid_bar_row <<- function(original, left, right, top, bottom) {
			val <- (left <= rightMouse) && (right >= leftMouse) && (min(bottom) <= topMouse) && (max(top) >= bottomMouse)
			if(val) {
				1
			} else{
				# original
				0
			}
		}

		.bars_info$data <<- ddply(.bars_info$data, .(label), transform, .brushed = valid_bar_row(.brushed, left, right, top, bottom))

		print(head(subset(.bars_info$data, .brushed > 0)))
		cat("count of brushed sections: ", sum(.bars_info$data$.brushed), "\n")
		cat("count of left(<=", rightMouse,"): ", sum(.bars_info$data$left <= rightMouse), #" - ",
			# paste(rownames(.bars_info$data[.bars_info$data$left <= rightMouse,]), sep = ", "),
			"\n")
		cat("count of right(>= ", leftMouse,"): ", sum(.bars_info$data$right >= leftMouse), #" - ",
			# paste(rownames(.bars_info$data[.bars_info$data$right >= leftMouse,]), sep = ", "),
			"\n")
		cat("count of bottom(<= ", topMouse,"): ", sum(.bars_info$data$bottom <= topMouse), #" - ",
			# paste(rownames(.bars_info$data[.bars_info$data$bottom <= topMouse,]), sep = ", "),
			"\n")
		cat("count of top(>= ", bottomMouse,"): ", sum(.bars_info$data$top >= bottomMouse), #" - ",
			# paste(rownames(.bars_info$data[.bars_info$data$top >= bottomMouse,]), sep = ", "),
			"\n")
		cat("setHiliting - done\n")
	}

	setSelected <- function() {
		cat("setSelected\n")
		section <- subset(.bars_info$data, .brushed == 1)

		rows <- rep(FALSE, nrow(mf_data))
		if(NROW(section) > 0)
			rows <- .mf_data_col_names  %in% as.character(section$label)
		
# update original data
		data$.brushed <- rows
		# print(mf_data[1:min(12, nrow(mf_data)), ])
		cat("setSelected - done\n")
	}


	#######################################################
	# Hover
	bar_hover_draw <- function(item, painter, exposed, ...) {
		cat("\nBar Hover Draw\n")
		# Don't draw when brushing
		if (is.null(.bar_queryPos)) return()

		if (horizontal) {
			x <- .bar_queryPos[2]
			y <- .bar_queryPos[1]
		} else {
			x <- .bar_queryPos[1]
			y <- .bar_queryPos[2]
		}

		section <- subset(.bars_info$data, (y <= top) & (y >= bottom) & (x <= right) & (x >=left))
		# print(head(section))

		# Nothing under mouse
		if (nrow(section) == 0){
			.bar_hover_section <<- list(top = -1, bottom = 1, right = -1, left = 1)
			return()
		}


		# Highlight the rect
		brushColor <- brush_attr(mf_data, ".brushed.color")
		if (horizontal) {
			qdrawRect(painter,
				xleft = c(section$bottom), #left
				ybottom = c(section$right), # bottom
				xright = c(section$top), # right
				ytop = c(section$left), # top
				stroke = brushColor,
				fill = c(NA)# fill
			)
		} else {
			qdrawRect(painter,
				xleft = c(section$left), #left
				ybottom = c(section$bottom), # bottom
				xright = c(section$right), # right
				ytop = c(section$top), # top
				stroke = brushColor,
				fill = c(NA)# fill
			)
		}

		# Work out label text
		infostring <- paste("\nbin:", section[1,"label"], sep = " ")
		if (splitByCol != "qhist_split_column") {
			infostring <- paste(infostring, "group:", section[1,"group"], sep = " ")
		}

		count <- section$top - section$bottom
		infostring <- paste(infostring, "count:", section[1,"count"], sep = " ")
		if (splitByCol != "qhist_split_column") {
			# nrow(section[]
			val <- section[1,"count"] / sum(.bars_info$data[.bars_info$data$label %in% section[1, "label"], "count"])
			if(val != 1) {
				infostring <- paste(
					infostring,
					"\ncolumn proportion: ",
					pretty_percent(val),
					sep = "")
			}
		}

		infostring <- paste(infostring, "\ndata proportion:", pretty_percent(section[1,"count"] / nrow(mf_data)), sep = " ")



		# Label -> bin
		# count
		# proportion
		#
		# when split...
		# Label -> bin
		# count
		# column proportion
		# section proportion of column

		qstrokeColor(painter) <- "white"
		qdrawText(painter, infostring, .bar_queryPos[1], .bar_queryPos[2], valign="top", halign="left")

		.bar_hover_section <<- list(top = section$top, bottom = section$bottom, left = section$left, right = section$right)
	}

	bar_hover <- function(item, event, ...) {
		# if (.brush) return()

		.bar_queryPos <<- as.numeric(event$pos())
		# qupdate(querylayer)

		cat("\nBar Hover\n")

		if (horizontal) {
			x <- .bar_queryPos[2]
			y <- .bar_queryPos[1]
		} else {
			x <- .bar_queryPos[1]
			y <- .bar_queryPos[2]
		}
		cat("x: ", x, "\ty: ", y, "\n")
		cat("top: ", .bar_hover_section$top, "\tbottom: ", .bar_hover_section$bottom, "\tleft: ", .bar_hover_section$left, "\tright: ", .bar_hover_section$right, "\n")

		if ( !((y <= .bar_hover_section$top) & (y >= .bar_hover_section$bottom) & (x <= .bar_hover_section$right) & (x >= .bar_hover_section$left))) {
			qupdate(hoverlayer)
		}
	}

	bar_leave <- function(item, event, ...) {
		# cat("\nBar Leave\n")
		# print(as.numeric(event$pos()))
		qupdate(hoverlayer)
	}


	#######################################################
	# Layout
	updateLims <- function() {
		windowRanges <- make_window_ranges(.ranges, .xlab, .ylab)
		.lims <<- qrect(windowRanges[c(1,2)], windowRanges[c(3,4)])
	}
	updateLims()

	.scene <- qscene()

	bglayer <- qlayer(.scene, coords, limits = .lims, clip = FALSE, keyPressFun = keyPressFun)

	datalayer <- qlayer(.scene, hist.all, limits = .lims, clip = FALSE)

	hoverlayer <- qlayer(.scene, bar_hover_draw, limits = .lims, clip = FALSE,
		hoverMoveFun = bar_hover, hoverLeaveFun = bar_leave)

	brushing_layer <- qlayer(.scene, brushing_draw,
		mousePressFun = brushing_mouse_press, mouseMoveFun = brushing_mouse_move,
		mouseReleaseFun = brushing_mouse_release,
		limits = .lims, clip = FALSE
	)


#	# update the brush layer in case of any modifications to the mutaframe
	add_listener(mf_data, function(i,j) {
		if (j == ".brushed") {
			.updateinfo <<- TRUE
			qupdate(brushing_layer)
		}
	})
	
#	add_listener(.brush.attr, function(i, j) {
#			qupdate(brushing_layer)
#	})

	.view <- qplotView(scene = .scene)
	.view
}
