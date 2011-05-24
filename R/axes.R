qaxis = function(parent = NULL, data = NULL, side = 1, at = NULL, labels = NULL,
    limits, ...) {
    lims = if (side%%2)
        qrect(limits, c(0, 1))
    else qrect(c(0, 1), limits)
    draw_axis = function(layer, painter) {
        if (is.null(at)) {
            at = .axis.loc(data)
        }
        if (is.null(labels)) {
            labels = if (!is.null(data) && is.factor(data))
                levels(data)
            else format(at)
        }
        xat = yat = at
        xalign = yalign = "center"
        xshift1 = yshift1 = xshift2 = yshift2 = 0
        ## side = 1, 2, 3, 4
        switch(side, {
            yat = 0.9
            yalign = "top"
            yshift1 = 0.01
            yshift2 = 0.1
        }, {
            xat = 0.9
            xalign = "right"
            xshift1 = 0.01
            xshift2 = 0.1
        }, {
            yat = 0.1
            yalign = "bottom"
            yshift1 = -0.01
            yshift2 = -0.1
        }, {
            xat = 0.1
            xalign = "left"
            xshift1 = -0.01
            xshift2 = -0.1
        })
        qdrawText(painter, labels, x = xat, y = yat, halign = xalign, valign = yalign)
        qdrawSegment(painter, xat + xshift1, yat + yshift1, xat + xshift2, yat +
            yshift2)
    }
    qlayer(parent, paintFun = draw_axis, limits = lims, ...)
}

.axis.loc = function(data) {
    if (is.factor(data)) {
        return(as.integer(data))
    }
    at = pretty(data)
    at[at <= max(data) & at >= min(data)]
}

qgrid = function(parent = NULL, xat, yat, xlim, ylim, ...) {
    ## background color
    .bgcolor = "grey80"

    draw_grid = function(layer, painter) {
        qdrawRect(painter, xlim[1], ylim[1], xlim[2], ylim[2], stroke = .bgcolor,
            fill = .bgcolor)
        qlineWidth(painter) = 1
        qdrawSegment(painter, xat, ylim[1], xat, ylim[2], stroke = "white")
        qdrawSegment(painter, xlim[1], yat, xlim[2], yat, stroke = "white")
        ## minor grid
        qlineWidth(painter) = 0.1
        xat = (xat[-1] + xat[-length(xat)])/2
        qdrawSegment(painter, xat, ylim[1], xat, ylim[2], stroke = "white")
        yat = (yat[-1] + yat[-length(yat)])/2
        qdrawSegment(painter, xlim[1], yat, xlim[2], yat, stroke = "white")
    }
    qlayer(parent, paintFun = draw_grid, limits = qrect(xlim, ylim), ...)
}


#' draw grid with qt
#' draws the grid at given positions
#' can be used as part of a recall function to update a particular layer
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
draw_grid_with_positions_fun <- function(plotObj, dataRanges, horiPos = NULL,
    vertPos = NULL, minor.horiPos = NULL, minor.vertPos = NULL) {
    #background
    qdrawRect(plotObj, dataRanges[1], dataRanges[3], dataRanges[2], dataRanges[4],
        fill = "grey80", stroke = "grey80")

    #horizontal
    if (!is.null(vertPos)) {
        nvlines <- length(vertPos)
        vdiff <- vertPos[nvlines] - vertPos[nvlines-1]
        qlineWidth(plotObj) <- 2

        qdrawLine(plotObj, x = rep(c(dataRanges[1:2], NA), nvlines), y = rep(vertPos,
            each = 3), stroke = "white")
        if (dataRanges[4] > (vertPos[nvlines] + vdiff/2))
            minor.vertPos <- vertPos + vdiff/2
        else
            minor.vertPos <- vertPos[-nvlines] + vdiff/2
    }

    #vertical
    if (!is.null(horiPos)) {
        nhlines <- length(horiPos)
        hdiff <- horiPos[nhlines] - horiPos[nhlines-1]
        qlineWidth(plotObj) <- 2

        qdrawLine(plotObj, x = rep(horiPos, each = 3), y = rep(c(dataRanges[3:4],
            NA), length(horiPos)), stroke = "white")

        if (dataRanges[2] > (horiPos[nhlines] + hdiff/2))
            minor.horiPos <- horiPos + hdiff/2
        else
            minor.horiPos <- horiPos[-nhlines] + hdiff/2
    }

    #minor horizontal
    if (!is.null(minor.vertPos)) {
        # change linewidth to smaller width
        qlineWidth(plotObj) <- 0.1
        qdrawLine(plotObj, x = rep(c(dataRanges[1:2], NA), length(minor.vertPos)),
            y = rep(minor.vertPos, each = 3), stroke = "white")

    }

    #minor vertical
    if (!is.null(minor.horiPos)) {

        # change linewidth to smaller width
        qlineWidth(plotObj) <- 0.1
        qdrawLine(plotObj, x = rep(minor.horiPos, each = 3), y = rep(c(dataRanges[3:4],
            NA), length(minor.horiPos)), stroke = "white")
    }



}

#' draw x axes with qt
#' draws the x axes with the labels and label positions given
#' can be used as part of a recall function to update a particular layer
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @param axisLabels vector of labels
#' @param labelHoriPos horizontal position of the axisLabels
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
draw_x_axes_with_labels_fun <- function(plotObj, dataRanges, axisLabels,
    labelHoriPos, name = NULL) {
    #  X label
    x_left <- range(dataRanges[1:2])
    x_bottom <- c(dataRanges[3], dataRanges[3])
    x_bottom <- x_bottom - 0.03 * diff(dataRanges[3:4])
    x_labelpos <- dataRanges[3] - 0.03 * diff(dataRanges[3:4])

    #  plotObj$add_layer(line(left=x_left,bottom=x_bottom,stroke='grey'))
    # draw tick marks
    qdrawLine(plotObj, x = rep(labelHoriPos, each = 3), y = rep(c(dataRanges[3],
        dataRanges[3] - 0.02 * diff(dataRanges[3:4]), NA), length(labelHoriPos)),
        stroke = "grey30")

    qstrokeColor(plotObj) <- "grey30"
    qdrawText(plotObj, text = axisLabels, x = labelHoriPos, y = x_labelpos, valign = "top")

    if (!is.null(name)) {
        qstrokeColor(plotObj) <- "black"
        qdrawText(plotObj, text = name, x = x_left[1] + 0.5 * diff(x_left), y = dataRanges[3] -
            0.13 * diff(dataRanges[3:4]), valign = "center")
    }


    #  x_axisLabels <- axisLabels
    #  bprint(x_left)
    #  bprint(x_bottom)
    #  bprint(x_labelpos)
    #  bprint(x_axisLabels)
    #  bprint(labelHoriPos)

}

#' draw y axes with qt
#' draws the y axes with the labels and label positions given
#' can be used as part of a recall function to update a particular layer
#'
#' @param plotObj Qt plot object to have the layer added to
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @param axisLabels vector of labels
#' @param labelVertPos vertical position of the axisLabels
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
draw_y_axes_with_labels_fun <- function(plotObj, dataRanges, axisLabels,
    labelVertPos, name = NULL) {
    # Y label
    y_left <- dataRanges[1] - 0.03 * diff(dataRanges[1:2])
    y_bottom = dataRanges[3:4]

    #  y_bottom <- range(y_bottom[y_bottom >= 0 && y_bottom < windowRanges[4]])
    y_labelpos = dataRanges[1] - 0.04 * diff(dataRanges[1:2])
    # use qstrWidth for label position?

    #draw x and y axes!
    qdrawLine(plotObj, x = rep(c(dataRanges[1] - 0.02 * diff(dataRanges[1:2]), dataRanges[1],
        NA), length(labelVertPos)), y = rep(labelVertPos, each = 3), stroke = "grey30")


    qstrokeColor(plotObj) <- "grey30"
    qdrawText(plotObj, text = axisLabels, x = y_labelpos, y = labelVertPos, halign = "right")

    if (!is.null(name)) {
        qstrokeColor(plotObj) <- "black"
        qdrawText(plotObj, text = name, x = dataRanges[1] - 0.18 * diff(dataRanges[1:2]),
            y = y_bottom[1] + 0.5 * diff(y_bottom), valign = "center", rot = 90)
    }

    #  y_axisLabels <- axisLabels
    #  bprint(y_left)
    #  bprint(y_bottom)
    #  bprint(y_labelpos)
    #  bprint(y_axisLabels)
    #  bprint(labelVertPos)

}

#' add a title using qt
#'
add_title_fun <- function(plotObj, dataRanges, title) {

    if (!is.null(title)) {
        qstrokeColor(plotObj) <- "black"
        qdrawText(plotObj, text = title, x = dataRanges[1] + 0.5 * diff(dataRanges[1:2]),
            y = dataRanges[4] + 0.05 * diff(dataRanges[3:4]), valign = "top")
    }
}

