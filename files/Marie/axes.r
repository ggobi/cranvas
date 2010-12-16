#bprint <- function(...) {
#  ignore <- ...
#}
#source("../Barret/bprint.r")

#' pretty axes
#' make 'pretty' axes and make sure that they are within the minimum and maximum
#'
#' @param dataRange data range that should be 'pretty'
#' @param minimum minimum value of the 'pretty' range
#' @param maximum maximum value of the 'pretty' range

make_pretty_axes <- function(dataRange, minimum, maximum) {
  prettyness <- pretty(dataRange)
  prettyness <- prettyness[prettyness >= minimum]
  prettyness <- prettyness[prettyness <= maximum]
  
  bprint(prettyness)
  prettyness
}

#' background & grid
#' draw a set of layers containing the background and gridlines at user-determined positions
#'
#' @param plotObj cranvas-plot object that provides access to root, scene and limit elements to which subsequent layers are attached
#' @param border_x white space outside of the background along x-axis, in data units
#' @param border_y white space outside of the background along y-axis, in data units
#' @param horiPos where to place the horizontal major gridlines, in data units
#' @param vertPos where to place the vertical major gridlines, in data units
#' @param row the row to place this set of layers
#' @param col the col to place this set of layers
#' @param geometry size of this set of layers
#' @author Marie Vendettuoli \email{mariev@@iastate.edu}

draw_grid_with_positions <- function(
  plotObj, 
  border_x,
  border_y,
  horiPos = NULL, 
  vertPos = NULL, 
  minor.horiPos = NULL, 
  minor.vertPos = NULL,
  row = 0,
  col = 0,
  geometry = qrect(0,0,600,400)) {

  #background: one grey rectangle 
  add_layer(parent = plotObj, mark = rect( left = border_x, 
                                           right = border_x * 0.25,
                                           bottom = border_y, 
                                           top = border_y * 0.75,
                                           fill = "grey90",
                                           stroke  = "grey90", 
                                           parent = plotObj),row = row, col = col, geometry = geometry)

  #vertical major grid lines
  if (!is.null(vertPos)) {
    add_layer(parent = plotObj, mark = vbar( left = vertPos,
                                             stroke  = "white",
                                             parent = plotObj,
                                             width = 2), row = row, col = col, geometry = geometry)

    minor.vertPos <- vertPos[-length(vertPos)] + diff(vertPos) / 2
   }

  #horizontal major grid lines
  if (!is.null(horiPos)) {
    add_layer(parent = plotObj, mark = hbar( bottom  = horiPos,
                                             stroke  = "white",
                                             parent = plotObj,
                                             width = 2), row = row, col = col, geometry = geometry)
    minor.horiPos <- horiPos[-length(horiPos)] + diff(horiPos)/2
  }

  #minor horizontal
  if (!is.null(minor.horiPos)) {	
    add_layer(parent = plotObj, mark = hbar( bottom = minor.horiPos, 
                                             stroke = "white",
                                             width = 1,
                                             parent = plotObj),row=row, col=col, geometry = geometry)

  }

  #minor vertical
  if (!is.null(minor.vertPos)) { 
    add_layer( parent = plotObj, mark = vbar( left =minor.vertPos,
                                              stroke  = "white",
                                              width   = 1,
                                              parent = plotObj), row = row, col = col, geometry = geometry)
  }
}


#' draw x axes
#' draws the x axes with the labels and label positions given
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @param axisLabels vector of labels 
#' @param labelHoriPos horizontal position of the axisLabels
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
  draw_x_axes_with_labels <- function(plotObj, dataRanges, axisLabels, labelHoriPos, name = NULL,row = 0L,col = 0L, xspan = NULL, yspan = NULL, geometry = qrect(0,0,600,400)) {
  #  X label
  x_left <- range(dataRanges[1:2])
  x_bottom <- c(dataRanges[3],dataRanges[3])
  x_bottom <- x_bottom - 0.03 * diff(dataRanges[3:4])
  x_labelpos <- dataRanges[3] - 0.03 * diff(dataRanges[3:4])

# draw tick marks, x axis
  add_layer(parent = plotObj, mark = vbar( left = labelHoriPos - plotObj$limits$left(),
                                           bottom = (0.1*plotObj$limits$height()),
                                           top = 0.78*plotObj$limits$height(),
                                           stroke = "grey30",
                                           parent = plotObj), row = row, col = col, geometry = geometry)
  
  #draw data labels, x axis
  add_layer(parent = plotObj, mark = text( text = axisLabels,
                                           left = labelHoriPos - plotObj$limits$left(),
                                           bottom = 0.1 * yspan,
                                           stroke = "grey30",
                                           valign = "top",
                                           parent = plotObj), row = row, col = col, geometry = geometry)

  #draw x axis label  
  if (!is.null(name)) {
    add_layer( parent = plotObj, mark = text( text = name,
                                              left = x_left[1] + 0.5 * diff(x_left),
                                              bottom = dataRanges[3] - 0.13 * diff(dataRanges[3:4]),
                                              stroke = "black",
                                              valign = "center",
                                              parent = plotObj), row = row, col = col, geometry = geometry)  
  }
}
#' draw y axes
#' draws the y axes with the labels and label positions given
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @param axisLabels vector of labels 
#' @param labelVertPos vertical position of the axisLabels
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
draw_y_axes_with_labels <- function(plotObj, dataRanges, axisLabels, labelVertPos, name = NULL,row = 0L,col = 0L, xspan = NULL, yspan = NULL, geometry = qrect(0, 0, 600, 400)) { 
  # Y label
  y_left <- dataRanges[1] - 0.03 * diff(dataRanges[1:2])
  y_bottom = dataRanges[3:4]

#  y_bottom <- range(y_bottom[y_bottom >= 0 && y_bottom < windowRanges[4]])
  y_labelpos = dataRanges[1] - 0.04 * diff(dataRanges[1:2])
# use qstrWidth for label position?

  # draw axis
  add_layer(parent = plotObj, mark = hbar( left = 0.15 * xspan,
#                                           right = .915 * xspan,
                                           right = 0.86 * xspan,
                                           bottom = labelVertPos - plotObj$limits$top(),
                                           stroke = "grey30",
                                           parent = plotObj), row = row, col = col, geometry = geometry)
  # draw labels
  add_layer(parent = plotObj, mark = text( text = axisLabels, 
                                           right = .87 *xspan, 
                                           bottom = labelVertPos - plotObj$limits$top(), 
                                           stroke = "grey30",
                                           halign = "right",
                                           valign = "center",
                                           parent = plotObj), row = row, col = col, geometry = geometry)
  
  if (!is.null(name)) {
    add_layer(parent = plotObj, mark = text( text = name,
                                             left = dataRanges[1] - 0.18 * diff(dataRanges[1:2]),
                                             bottom = y_bottom[1] + 0.5 * diff(y_bottom),
                                             stroke = "black",
                                             valign = "center",
                                             rot = 90,
                                             parent = plotObj), row = row, col = col, geometry = geometry)  
  }
}

draw_x_facet_labels <- function (plotObj, border_x, yspan, xspan,row = 0L, col = 0L, geometry = qrect(0, 0, 600, 400), name) {
  add_layer (parent = plotObj, mark = rect( left = border_x, 
                                            right = border_x * 0.25,
                                            bottom = 0.93 * yspan,
                                            top = 0,
                                            fill = "grey80",
                                            stroke  = "grey80",
                                            parent = plotObj), row = row, col = col, geometry = geometry)
  add_layer (parent = plotObj, mark = text (text = name,
                                            left = 0.5 * xspan,
                                            top = 0.04 * yspan,
                                            stroke = "black",
                                            valign = "center",
                                            parent = plotObj), row = row, col = col, geometry = geometry)

}

draw_y_facet_labels <- function (plotObj, border_y, xspan, yspan, row = 0L, col =0L, geometry = qrect(0,0,600, 400), name) {
  add_layer(parent = plotObj, mark = rect( left = .95* xspan,
                                           right = 0,
                                           bottom = border_y,
                                           top = border_y * 0.75,
                                           fill = "grey80",
                                           stroke = "grey80",
                                           parent = plotObj), row = row, col = col, geometry = geometry)
  add_layer(parent = plotObj, mark = text (text = name,
                                           left = 0.94*xspan,
                                           bottom = 0.5 *  yspan,
                                           stroke = "black",
                                           rot = -90,
                                           valign = "bottom",
                                           parent = plotObj), row = row, col = col, geometry = geometry)
}

draw_grid_with_positions_fun <- function(
  plotObj, 
  dataRanges, 
  horiPos = NULL, 
  vertPos = NULL, 
  minor.horiPos = NULL, 
  minor.vertPos = NULL
) {

  #background  
    qdrawRect(plotObj,
    
      dataRanges[1], 
      dataRanges[3],
      dataRanges[2], 
      dataRanges[4],
      fill    = "grey80",
      stroke  = "grey80"
    )

  #horizontal
  if (!is.null(vertPos)) {
  	qlineWidth(plotObj) <- 2
 
    qdrawLine(
      plotObj,
      x      = rep(c(dataRanges[1:2],NA), length(vertPos)),
      y      = rep(vertPos,each=3),
      stroke = "white"
    )

  	minor.vertPos <- vertPos[-length(vertPos)] + diff(vertPos)/2
  }

  #vertical
  if (!is.null(horiPos)) {
  	qlineWidth(plotObj) <- 2
 
    qdrawLine(
      plotObj,
      x       = rep(horiPos,each=3),
      y       = rep(c(dataRanges[3:4],NA), length(horiPos)),
      stroke  = "white"
    )

    minor.horiPos <- horiPos[-length(horiPos)] + diff(horiPos)/2
  }

  #minor horizontal
  if (!is.null(minor.vertPos)) {	
    # change linewidth to smaller width
    qlineWidth(plotObj) <- 0.1
    qdrawLine(
      plotObj,
      x      = rep(c(dataRanges[1:2],NA), length(minor.vertPos)),
      y      = rep(minor.vertPos,each=3),
      stroke = "white"
    )

  }

  #minor vertical
  if (!is.null(minor.horiPos)) { 
  
    # change linewidth to smaller width
	qlineWidth(plotObj) <- 0.1
    qdrawLine(
      plotObj,
      x      = rep(minor.horiPos,each=3),
      y      = rep(c(dataRanges[3:4],NA), length(minor.horiPos)),
      stroke = "white"
    )
  }



}


draw_x_axes_fun <- function(plotObj, dataRanges, name = NULL) {
  xRangeLabels <- make_pretty_axes(dataRanges[1:2], dataRanges[1], dataRanges[2])
  draw_x_axes_with_labels_fun(plotObj, dataRanges, xRangeLabels, xRangeLabels, name)
}


draw_x_axes_with_labels_fun <- function(plotObj, dataRanges, axisLabels, labelHoriPos, name = NULL) {
  #  X label
  x_left <- range(dataRanges[1:2])
  x_bottom <- c(dataRanges[3],dataRanges[3])
  x_bottom <- x_bottom - 0.03 * diff(dataRanges[3:4])
  x_labelpos <- dataRanges[3] - 0.03 * diff(dataRanges[3:4])

#  plotObj$add_layer(line(left=x_left,bottom=x_bottom,stroke="grey"))
# draw tick marks
print("labelHoriPos")
print(labelHoriPos)
  qdrawLine(
    plotObj,
  	x = rep(labelHoriPos,each=3),
  	y = rep(c(dataRanges[3],dataRanges[3] - 0.02 * diff(dataRanges[3:4]),NA), length(labelHoriPos)),
  	stroke = "grey30")
  
  qstrokeColor(plotObj) <- "grey30"
  qdrawText(
    plotObj,
    text = axisLabels,
    x = labelHoriPos,
    y = x_labelpos, 
    valign = "top"
  )
  
  if (!is.null(name)) {
    qstrokeColor(plotObj) <- "black"
 	  qdrawText(
      plotObj,
      text = name,
      x = x_left[1] + 0.5 * diff(x_left),
      y = dataRanges[3] - 0.13 * diff(dataRanges[3:4]),
      valign = "center"        
    )
  }  

  
#  x_axisLabels <- axisLabels
#  bprint(x_left)
#  bprint(x_bottom)
#  bprint(x_labelpos)
#  bprint(x_axisLabels)
#  bprint(labelHoriPos)

}

draw_y_axes_with_labels_fun <- function(plotObj, dataRanges, axisLabels, labelVertPos, name = NULL) {
  # Y label
  y_left <- dataRanges[1] - 0.03 * diff(dataRanges[1:2])
  y_bottom = dataRanges[3:4]

#  y_bottom <- range(y_bottom[y_bottom >= 0 && y_bottom < windowRanges[4]])
  y_labelpos = dataRanges[1] - 0.04 * diff(dataRanges[1:2])
# use qstrWidth for label position?

  #draw x and y axes!
  qdrawLine(
    plotObj,
  	x = rep(c(dataRanges[1] - 0.02 * diff(dataRanges[1:2]), dataRanges[1],NA), length(labelVertPos)),
  	y = rep(labelVertPos,each=3),
  	stroke = "grey30"
  )
  
  
  qstrokeColor(plotObj) <- "grey30"
  qdrawText(
    plotObj,
    text = axisLabels, 
    x = y_labelpos, 
    y = labelVertPos, 
    halign = "right"
  )
  
  if (!is.null(name)) {
  	qstrokeColor(plotObj) <- "black"
    qdrawText(plotObj,
      text = name,
      x = dataRanges[1] - 0.18 * diff(dataRanges[1:2]),
      y = y_bottom[1] + 0.5 * diff(y_bottom),
      valign = "center",
      rot=90
    )
  }  

#  y_axisLabels <- axisLabels
#  bprint(y_left)
#  bprint(y_bottom)
#  bprint(y_labelpos)
#  bprint(y_axisLabels)
#  bprint(labelVertPos)

}


