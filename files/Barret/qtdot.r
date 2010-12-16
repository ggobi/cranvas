#' Create a dot plot
#' Create a dot plot from 1-D numeric data
#'
#' http://content.answcdn.com/main/content/img/oxford/Oxford_Statistics/0199541454.dot-plot.1.jpg
#'
#' @param data vector of numeric data to be made into a histogram
#' @param horizontal boolean to decide if the bars are horizontal or vertical
#' @param ... arguments supplied to hist() or the hist layer
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  # toture
#'    qdot(rnorm(1000000), floor(rnorm(1000000)*3))
#'
#'		# each column is split evenly
#'    qdot(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - stack") 
#'
#'		# each column has similar height colors
#'    qdot(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - dodge", position = "dodge") 
#'
#'		# range from 0 to 1
#'    qdot(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - relative", position = "relative") 
#'
#'  # color tests
#'		# all color is defined
#'    qdot(mtcars$disp, horizontal = TRUE, fill = "gold", stroke = "red4")
#'
#'		# stacked items
#'    qdot(mtcars$disp, mtcars$cyl, stroke = "black", position = "stack")
#'
#'		# raw value items
#'    qdot(mtcars$disp, mtcars$cyl, stroke = "black", position = "identity")
#'
#'		# dodged items
#'    qdot(mtcars$disp, mtcars$cyl, stroke = "black", position = "dodge")
#'
#'		# range from 0 to 1
#'    qdot(mtcars$disp, mtcars$cyl, stroke = "black", position = "relative")
qdot <- function(
  data, 
  splitBy = rep(1, length(data)), 
  horizontal = TRUE,
  position = "none",
  color = NULL,
  fill = NULL,
  stroke = NULL,
  title = NULL,
  name = names(data),
  ...
) {

  bars_info <- continuous_to_bars(data, splitBy, position, color, fill, stroke, ...)
  bars <- bars_info$data
  color <- bars$color  

    
  # contains c(x_min, x_max, y_min, y_max)
  if (horizontal) {
    ranges <- c(make_data_ranges(c(0, bars$top)), make_data_ranges(bars_info$breaks))
  } else {
    ranges <- c(make_data_ranges(bars_info$breaks), make_data_ranges( c(0, bars$top)))
  }
  
  if (horizontal) {
    ylab = name
    xlab = "count"
  } else {
    ylab = "count"
    xlab = name
  }

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
	
	dot.all <- function(item, painter, exposed) {
		
		if (horizontal) {
			qdrawRect(painter,
				xleft = c(bars$bottom), #left
				ybottom = c(bars$left), # bottom 
				xright = c(bars$top), # right
				ytop = c(bars$right), # top
				stroke = c(bars$stroke),
				fill = c(bars$fill)# fill
			)
		} else {
			qdrawRect(painter,
				xleft = c(bars$left), #left
				ybottom = c(bars$bottom), # bottom 
				xright = c(bars$right), # right
				ytop = c(bars$top), # top
				stroke = c(bars$stroke),
				fill = c(bars$fill)# fill
			)
		}
	}	
	
	windowRanges <- make_window_ranges(ranges, xlab, ylab)
	lims <- qrect(windowRanges[c(1,2)], windowRanges[c(3,4)])
	
	scene = qscene()
	
	bglayer = qlayer(scene, coords, limits = lims, clip = FALSE
		# , keyPressFun=keyPressFun
	)

	datalayer = qlayer(scene, dot.all, limits = lims, clip = FALSE)

	# brushing_layer = qlayer(scene, brushing_draw, 
	# 	# mousePressFun = brushing_mouse_press, mouseMoveFun = brushing_mouse_move,  
	# 	# mouseReleaseFun = brushing_mouse_release, 
	# 	limits = lims, clip = FALSE
	# )

	# querylayer = qlayer(scene, query_draw, limits = lims, clip = FALSE,
	# 	# hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave
	# )

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



# # create the plot
# # window size 600 x 600; xrange and yrange from above
# windowRanges <- make_window_ranges(ranges, xlab, ylab)
# plot1<-make_new_plot(windowRanges)
# 
#   
# 
# # for different representations of the data (shape, color, etc) pass vecor arguments for shape, color, x, y
# # c(obj) makes a matrix into a vector
# if (horizontal) {
#   plot1$add_layer(
# 			hbar(
# 				bottom = c(bars$left), 
# 				top = c(bars$right), 
# 				width = c(bars$top), 
# 				left = c(bars$bottom), 
# 				fill = c(bars$fill), 
# 				stroke = c(bars$stroke)
# 			)
# 		)
# } else {
#   plot1$add_layer(
# 			vbar(
# 				left = c(bars$left), 
# 				right = c(bars$right), 
# 				height = c(bars$top), 
# 				bottom = c(bars$bottom), 
# 				fill = c(bars$fill), 
# 				stroke = c(bars$stroke)
# 			)
# 		)
# }
# 
# draw_x_axes(plot1, ranges, xlab)
# draw_y_axes(plot1, ranges, ylab) 
# 
# if(!is.null(title))
#   add_title(plot1, ranges, title)
# 
# plot1
