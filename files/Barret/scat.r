source("../utilities/api-sketch.r")
source("../utilities/axes.r")
source("../utilities/helper.r")
source("bprint.r")


#' Create a scatterplot
#' Create a scatterplot from numeric data
#'
#' @param x vector of numeric data
#' @param y vector of numeric data
#' @param ... arguments supplied to hist() or the hist layer
#' @param title title at the top of the plot
#' @param xlab x label
#' @param y y label
#' @param color color of both the fill and stroke
#' @param fill fill of the dots
#' @param stroke stroke of the dots
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  qtscat(iris$Petal.Width, iris$Petal.Length)
#'  qtscat(iris$Petal.Width, iris$Petal.Length, title = "Iris", xlab = "Petal Width", ylab = "Petal Length", fill = "gold", stroke = "red4")
qtscat <- function(x, y, ..., title=NULL, xlab = NULL, ylab = NULL, color=NULL, fill = NULL, stroke = NULL) {
  ranges <- c(make_data_ranges(x), make_data_ranges(y))
  bprint(ranges)
  #create the plot
  #window size 600 x 600; xrange and yrange from above
  windowRanges <- make_window_ranges(ranges, xlab, ylab)
  plot1<-make_new_plot(windowRanges)

  #draw grid
  draw_grid(plot1, ranges)
  
  #for different representations of the data (shape, color, etc) pass vecor arguments for shape, color, x, y
  if(is.null(color))
    color = "black"
  if(is.null(stroke))
    stroke = color
  if(is.null(fill))
    fill = color
    
  plot1$add_layer(glyph(left = x, bottom = y, fill=fill, stroke=stroke))

  draw_x_axes(plot1, ranges, xlab)
  draw_y_axes(plot1, ranges, ylab) 
  
  if(!is.null(title))
    add_title(plot1, ranges, title)

  plot1  
}