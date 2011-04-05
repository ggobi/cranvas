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
#'\t\t# each column is split evenly
#'    qdot(rnorm(1000000), floor(runif(1000000)*15), title = 'Toture - stack')
#'
#'\t\t# each column has similar height colors
#'    qdot(rnorm(1000000), floor(runif(1000000)*15), title = 'Toture - dodge', position = 'dodge')
#'
#'\t\t# range from 0 to 1
#'    qdot(rnorm(1000000), floor(runif(1000000)*15), title = 'Toture - relative', position = 'relative')
#'
#'  # color tests
#'\t\t# all color is defined
#'    qdot(mtcars$disp, horizontal = TRUE, fill = 'gold', stroke = 'red4')
#'
#'\t\t# stacked items
#'    qdot(mtcars$disp, mtcars$cyl, stroke = 'black', position = 'stack')
#'
#'\t\t# raw value items
#'    qdot(mtcars$disp, mtcars$cyl, stroke = 'black', position = 'identity')
#'
#'\t\t# dodged items
#'    qdot(mtcars$disp, mtcars$cyl, stroke = 'black', position = 'dodge')
#'
#'\t\t# range from 0 to 1
#'    qdot(mtcars$disp, mtcars$cyl, stroke = 'black', position = 'relative')
qdot <- function(data, splitBy = rep(1, length(data)), horizontal = TRUE, 
    position = "none", color = NULL, fill = NULL, stroke = NULL, title = NULL, name = names(data), 
    ...) {
    
    bars_info <- continuous_to_bars(data, splitBy, position, color, fill, stroke, 
        ...)
    bars <- bars_info$data
    color <- bars$color
    
    
    # contains c(x_min, x_max, y_min, y_max)
    if (horizontal) {
        ranges <- c(make_data_ranges(c(0, bars$top)), make_data_ranges(bars_info$breaks))
    }
    else {
        ranges <- c(make_data_ranges(bars_info$breaks), make_data_ranges(c(0, bars$top)))
    }
    
    if (horizontal) {
        ylab = name
        xlab = "count"
    }
    else {
        ylab = "count"
        xlab = name
    }
    
    coords <- function(item, painter, exposed) {
        # grey background with grid lines
        if (horizontal) {
            draw_grid_with_positions_fun(painter, ranges, horiPos = make_pretty_axes(ranges[1:2], 
                ranges[1], ranges[2]))
        }
        else {
            draw_grid_with_positions_fun(painter, ranges, vertPos = make_pretty_axes(ranges[3:4], 
                ranges[3], ranges[4]))
        }
        
        # put labels, if appropriate
        draw_x_axes_fun(painter, ranges, xlab)
        draw_y_axes_fun(painter, ranges, ylab)
        
        # title
        if (!is.null(title)) 
            add_title_fun(painter, ranges, title)
    }
    
    dot.all <- function(item, painter, exposed) {
        
        if (horizontal) {
            qdrawRect(painter, xleft = c(bars$bottom), ybottom = c(bars$left), xright = c(bars$top), 
                ytop = c(bars$right), stroke = c(bars$stroke), fill = c(bars$fill))
        }
        else {
            qdrawRect(painter, xleft = c(bars$left), ybottom = c(bars$bottom), xright = c(bars$right), 
                ytop = c(bars$top), stroke = c(bars$stroke), fill = c(bars$fill))
        }
    }
    
    windowRanges <- make_window_ranges(ranges, xlab, ylab)
    lims <- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])
    
    scene = qscene()
    
    bglayer = qlayer(scene, coords, limits = lims, clip = FALSE)
    
    datalayer = qlayer(scene, dot.all, limits = lims, clip = FALSE)
    
    # brushing_layer = qlayer(scene, brushing_draw,
    # \t# mousePressFun = brushing_mouse_press, mouseMoveFun = brushing_mouse_move,
    # \t# mouseReleaseFun = brushing_mouse_release,
    # \tlimits = lims, clip = FALSE
    # )
    
    # querylayer = qlayer(scene, query_draw, limits = lims, clip = FALSE,
    # \t# hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave
    # )
    
    # # update the brush layer in case of any modifications to the mutaframe
    # if (is.mutaframe(odata)) {
    # \tadd_listener(odata, function(i,j) {
    # \t\tif (j == '.brushed') {
    # \t\t\tqupdate(brushing_layer)
    # \t\t}
    # \t})
    # }
    # add_listener(.brush.attr, function(i, j) {
    # \t# wouldn't need to call recalchiliting ...
    # \tqupdate(brushing_layer)
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
# # for different representations of the data (shape, color, etc) pass vecor
#   arguments for shape, color, x, y
# # c(obj) makes a matrix into a vector
# if (horizontal) {
#   plot1$add_layer(
# \t\t\thbar(
# \t\t\t\tbottom = c(bars$left),
# \t\t\t\ttop = c(bars$right),
# \t\t\t\twidth = c(bars$top),
# \t\t\t\tleft = c(bars$bottom),
# \t\t\t\tfill = c(bars$fill),
# \t\t\t\tstroke = c(bars$stroke)
# \t\t\t)
# \t\t)
# } else {
#   plot1$add_layer(
# \t\t\tvbar(
# \t\t\t\tleft = c(bars$left),
# \t\t\t\tright = c(bars$right),
# \t\t\t\theight = c(bars$top),
# \t\t\t\tbottom = c(bars$bottom),
# \t\t\t\tfill = c(bars$fill),
# \t\t\t\tstroke = c(bars$stroke)
# \t\t\t)
# \t\t)
# }
#
# draw_x_axes(plot1, ranges, xlab)
# draw_y_axes(plot1, ranges, ylab)
#
# if(!is.null(title))
#   add_title(plot1, ranges, title)
#
# plot1 
