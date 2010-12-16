has_column <- function(data, col) {
	col %in% names(data)
}

column_coerce <- function(data, column, defaultVal) {

	if (!has_column(data, column)) {
		data[[column]] <- defaultVal
	}
	data
}


#' Create a hist plot
#' Create a hist plot from 1-D numeric data
#'
#' @param data vector of numeric data to be made into a histogram
#' @param horizontal boolean to decide if the bars are horizontal or vertical
#' @param ... arguments supplied to hist() or the hist layer
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples # toture
#'		rows <- 1000000
#'		bigData <- qmutaframe(data.frame(x = rnorm(rows), y = floor(rnorm(rows) * 7)))
#'		qhist(bigData)
#'
#'		# each column is split evenly
#'		qhist(bigData, splitByCol = "y", title = "Toture - stack")
#'		qhist(bigData, splitByCol = "y", title = "Toture - stack", horizontal = FALSE)
#'
#'		# each column has similar height colors
#'		qhist(bigData, splitByCol = "y", title = "Toture - dodge", position = "dodge")
#'
#'		# range from 0 to 1
#'		qhist(bigData, splitByCol = "y", title = "Toture - relative", position = "relative")
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
	...
) {

	if (splitByCol == -1) {
		splitByCol <- "qbar_split_column"
		data[[splitByCol]] <- 1
	}

	mf_data <- qmutaframe(data)
	n_data <- data.frame(mf_data)
	# print(head(mf_data))
	# print(head(n_data))
	.type <- "hist"
	.binwidth <- range(n_data[,xCol]) / 30
	.data_start_pos <- 0

	bars_info <- continuous_to_bars(data = n_data[,xCol], splitBy = n_data[, splitByCol], type = .type, position = position, color = color, fill = fill, stroke = stroke, ...)
	# bars <- bars_info$data
	# color <- bars$color


	# contains c(x_min, x_max, y_min, y_max)
	if (horizontal) {
		ranges <- c(make_data_ranges(c(0, bars_info$data$top)), make_data_ranges(bars_info$breaks))
	} else {
		ranges <- c(make_data_ranges(bars_info$breaks), make_data_ranges( c(0, bars_info$data$top)))
	}

	if (horizontal) {
		ylab = name
		xlab = "count"
	} else {
		ylab = "count"
		xlab = name
	}


	#######################################################
	# Draw Axes
	coords <- function(item, painter, exposed) {
		# grey background with grid lines
		if (horizontal) {
			draw_grid_with_positions_fun(painter, ranges, horiPos = make_pretty_axes(ranges[1:2], ranges[1], ranges[2]))
		} else {
			draw_grid_with_positions_fun(painter, ranges, vertPos = make_pretty_axes(ranges[3:4], ranges[3], ranges[4]))
		}

		# put labels, if appropriate
		draw_x_axes_fun(painter, ranges, xlab)
		draw_y_axes_fun(painter, ranges, ylab)

		# title
		if(!is.null(title))
			add_title_fun(painter, ranges, title)
	}


	#######################################################
	# Draw Bars
	hist.all <- function(item, painter, exposed) {
		if (horizontal) {
			qdrawRect(painter,
				xleft = c(bars_info$data$bottom), #left
				ybottom = c(bars_info$data$left), # bottom
				xright = c(bars_info$data$top), # right
				ytop = c(bars_info$data$right), # top
				stroke = c(bars_info$data$stroke),
				fill = c(bars_info$data$fill)# fill
			)
		} else {
			qdrawRect(painter,
				xleft = c(bars_info$data$left), #left
				ybottom = c(bars_info$data$bottom), # bottom
				xright = c(bars_info$data$right), # right
				ytop = c(bars_info$data$top), # top
				stroke = c(bars_info$data$stroke),
				fill = c(bars_info$data$fill)# fill
			)
		}
	}


	#######################################################
	# Key Functions
	keyPressFun <- function(item, event, ...) {
		# print(event$key())
		key <- event$key()

		# datachanged <- FALSE
		# formulachanged <- FALSE
		# form <- parse_product_formula(formula)

		altered <- FALSE
		if (key == Qt$Qt$Key_Up) {						# arrow up
			.binwidth <<- .binwidth * 1.10
			altered <- TRUE
		} else if (key == Qt$Qt$Key_Down) {		# arrow down
			.binwidth <<- .binwidth / 1.10
			altered <- TRUE
		} else if (key == Qt$Qt$Key_Left) {		# arrow left
			.data_start_pos <<- .data_start_pos + range(n_data[,xCol]) / 60
			altered <- TRUE
		} else if (key == Qt$Qt$Key_Right) {	# arrow left
			.data_start_pos <<- .data_start_pos - range(n_data[,xCol]) / 60
			altered <- TRUE
		} else if (key == Qt$Qt$Key_A) {			# 'c' or 'C' for 'condition'
			.type <<- "ash"
			altered <- TRUE
		} else if (key == Qt$Qt$Key_D) {			# 'c' or 'C' for 'condition'
			.type <<- "density"
			altered <- TRUE
		} else if (key == Qt$Qt$Key_O) {			# 'c' or 'C' for 'condition'
			.type <<- "dot"
			altered <- TRUE
		} else if (key == Qt$Qt$Key_H) {			# 'c' or 'C' for 'condition'
			.type <<- "hist"
			altered <- TRUE
		}

		if (altered) {
			# qupdate()
		}

	}
	#######################################################
	# Brushing
	.startBrush <- NULL
	.endBrush <- NULL
	.brush <- FALSE

	draw_brush_rect <- function(item, painter, exposed) {
		cat("\ndraw brush rect\n")
		left = min(.startBrush[1], .endBrush[1])
		right = max(.startBrush[1], .endBrush[1])
		top = max(.startBrush[2], .endBrush[2])
		bottom = min(.startBrush[2], .endBrush[2])

		qdrawRect(painter, left, bottom, right, top, fill=rgb(0,0,0,alpha=0.7), stroke="black")
		cat("\ndraw brush rect - done\n")
	}

	brushing_draw <- function(item, painter, exposed, ...) {
		cat("\nbrushing draw\n")
		if (TRUE) {
			# if (.brush) {
				#		if (is.null(data$.brushed))
				#          data$.brushed <- FALSE
				# print(str(bars_info$data))
				section <- subset(bars_info$data, (.brushed == TRUE))
			# }

			if (nrow(section) > 0) {
				#  .brush.attr = attr(odata, '.brush.attr')
				# brushcolor <- .brush.attr[,".brushed.color"]
				brushColor <- brush_attr(mf_data, ".brushed.color")
				if (horizontal)
					qdrawRect(painter, section$bottom, section$right, section$top, section$left, fill = brushColor, stroke = "black")
				else
					qdrawRect(painter, section$left, section$bottom, section$right, section$top, fill = brushColor, stroke = "black")
			}
		}

		if (!is.null(.endBrush)) {
			draw_brush_rect(item, painter, exposed)
		}
		cat("\nbrushing draw\n")
	}

	brushing_mouse_press <- function(item, event, ...) {
		cat("\nBrushing mouse press\n")
		.brush <<- TRUE
		if (is.null(.startBrush)) {
			.startBrush <<- as.numeric(event$pos())
		}
		qupdate(brushing_layer)
		cat("\nBrushing mouse press - done\n")
	}

	brushing_mouse_move <- function(item, event, ...) {
		cat("\nbrushing mouse move\n")
		.endBrush <<- as.numeric(event$pos())

		setHiliting()
		qupdate(brushing_layer)
		cat("\nbrushing mouse move - done\n")
	}

	brushing_mouse_release <- function(item, event, ...) {
		cat("\nbrushing mouse release\n")
		.endBrush <<- as.numeric(event$pos())
		setHiliting()
		qupdate(brushing_layer)


		.brush <<- FALSE
		.startBrush <<- NULL
		.endBrush <<- NULL

		setSelected()
		cat("\nbrushing mouse release - done\n")
	}

	setHiliting <- function() {
		cat("\nsetHiliting\n")

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

		# rows <<- (bars_info$data$left <= right) & (bars_info$data$right >= left) &
		# 	(bars_info$data$bottom <= top) & (bars_info$data$top >= bottom)
		# bars_info$data$.brushed <<- rows

		valid_bar_row <<- function(left, right, top, bottom) {
			(left <= rightMouse) & (right >= leftMouse) & (min(bottom) <= topMouse) & (max(top) >= bottomMouse)
		}

		bars_info$data <<- ddply(bars_info$data, .(label), transform, .brushed = valid_bar_row(left, right, top, bottom))

		print(head(subset(bars_info$data, .brushed == TRUE)))
		cat("count of brushed sections: ", sum(bars_info$data$.brushed), "\n")
		cat("count of left(<=", rightMouse,"): ", sum(bars_info$data$left <= rightMouse), " - ",
			# paste(rownames(bars_info$data[bars_info$data$left <= rightMouse,]), sep = ", "),
			"\n")
		cat("count of right(>= ", leftMouse,"): ", sum(bars_info$data$right >= leftMouse), " - ",
			# paste(rownames(bars_info$data[bars_info$data$right >= leftMouse,]), sep = ", "),
			"\n")
		cat("count of bottom(<= ", topMouse,"): ", sum(bars_info$data$bottom <= topMouse), " - ",
			# paste(rownames(bars_info$data[bars_info$data$bottom <= topMouse,]), sep = ", "),
			"\n")
		cat("count of top(>= ", bottomMouse,"): ", sum(bars_info$data$top >= bottomMouse), " - ",
			# paste(rownames(bars_info$data[bars_info$data$top >= bottomMouse,]), sep = ", "),
			"\n")

		cat("\nsetHiliting - done\n")
	}

	setSelected <- function() {
		section <- subset(bars_info$data, .brushed == TRUE, drop = FALSE)

		if (nrow(section) > 0) {

			starts <- unique(section$left)
			starts <- starts[order(starts)]
			ends <- unique(section$right)
			ends <- ends[order(ends)]

			n_data <- data.frame(mf_data)

			for (i in seq_along(starts)) {
				rows <- n_data[[xCol]] <= ends[i] & n_data[[xCol]] > starts[i]
				n_data$.brushed[rows] <- TRUE
			}

			print(head(n_data))
			print(head(subset(n_data, .brushed == TRUE)))

			# hdata$ID <- 1:nrow(section)
			# res.melt <- melt(hdata,id.var="ID")
			# res.cond <- adply(res.melt, 1, function(x) {
			# 	if (is.na(x$value)) cstr <- paste("is.na(",x$variable,")", sep="")
			# 	else cstr <- paste("(",x$variable,"=='",x$value,"')",sep="")
			# 	return(cond=cstr)
			# })
			#
			# res.cond <- res.cond[,-3]
			# names(res.cond)[3] <- "value"
			# cast.res <- cast(res.cond, ID~., function(x) return(paste(x, collapse=" & ")))
			#
			# cond1 <- paste("(",cast.res[,2],")", sep="",collapse=" | ")
			# idx <- with(data.frame(odata), which(eval(parse(text=cond1))))
			#
			# .brushed <- rep(FALSE, nrow(odata))
			# if (length(idx)) .brushed[idx] <- TRUE
			#
			# odata$.brushed <- .brushed
		} else {
			mf_data$.brushed <- FALSE
		}

		#   idx <- with(data.frame(odata), which(eval(parse(text=cond1))))
		#   return(idx)
	}


	#######################################################
	# Hover
	.bar_queryPos <- NULL
	.bar_hover_section <- list(top = -1, bottom = 1, right = -1, left = 1)
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

		section <- subset(bars_info$data, (y <= top) & (y >= bottom) & (x <= right) & (x >=left))
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
		if (splitByCol != "qbar_split_column") {
			infostring <- paste(infostring, "group:", section[1,"group"], sep = " ")
		}

		count <- section$top - section$bottom
		infostring <- paste(infostring, "count:", count, sep = " ")
		if (splitByCol != "qbar_split_column") {
			# nrow(section[]
			infostring <- paste(infostring, "column proportion:", count / 1, sep = " ")
		} else {
			infostring <- paste(infostring, "proportion:", count / nrow(n_data), sep = " ")
		}


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
		# if (.brush) return()
		#
		# .bar_queryPos <<- as.numeric(event$pos())
		# qupdate(querylayer)
		cat("\nBar Leave\n")
		print(as.numeric(event$pos()))
		qupdate(hoverlayer)
	}


	#######################################################
	# Layout
	windowRanges <- make_window_ranges(ranges, xlab, ylab)
	lims <- qrect(windowRanges[c(1,2)], windowRanges[c(3,4)])

	scene = qscene()

	bglayer = qlayer(scene, coords, limits = lims, clip = FALSE, keyPressFun=keyPressFun)

	datalayer = qlayer(scene, hist.all, limits = lims, clip = FALSE)

	hoverlayer = qlayer(scene, bar_hover_draw, limits = lims, clip = FALSE,
		hoverMoveFun = bar_hover, hoverLeaveFun = bar_leave)

	brushing_layer = qlayer(scene, brushing_draw,
		mousePressFun = brushing_mouse_press, mouseMoveFun = brushing_mouse_move,
		mouseReleaseFun = brushing_mouse_release,
		limits = lims, clip = FALSE
	)


	# # update the brush layer in case of any modifications to the mutaframe
	# if (is.mutaframe(odata)) {
	# 	add_listener(odata, function(i,j) {
	# 		if (j == ".brushed") {
	# 			qupdate(brushing_layer)
	# 		}
	# 	})
	# }
	# add_listener(.brush.attr, function(i, j) {
	# 	# wouldn't need to call recalchiliting ...
	# 	qupdate(brushing_layer)
	# })

	qplotView(scene = scene)
}