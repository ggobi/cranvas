##' Create an axis layer.
##' This function creates an axis layer which contains tick marks and
##' labels at given locations.
##' @param parent the parent layer (default to be \code{NULL}, which
##' means creating an independent layer with no parents, but it can be
##' added to a parent layer using the approach \code{parent[i, j] <-
##' child_layer})
##' @param meta \code{NULL} means to use \code{at} and \code{labels},
##' otherwise it should be a list containing child elements
##' \code{xat}, \code{yat}, \code{xlabels} and \code{ylabels}, and it
##' will override the arguments \code{at} and \code{labels}; besides,
##' another child element \code{meta$limits} must be present to set
##' the limits of the layer
##' @param side which side to draw the axis (following the convention
##' of R base graphics, i.e., 1: bottom, 2: left, 3: top, 4: right);
##' the location of tick marks and labels will automatically adjusted
##' according \code{side}
##' @param at the locations of tick marks
##' @param labels the labels of the tick marks
##' @param ... other arguments passed to \code{\link[qtpaint]{qlayer}}
##' @return a layer object
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @seealso \code{\link[graphics]{axis}}, \code{\link[qtpaint]{qlayer}}
##' @note The vertical range of the x-axis is [0, 1], and the
##' horizontal limit of y-axis is [0, 1].
##'
##' If \code{meta} is not \code{NULL}, it is supposed to be a
##' reference object, and an event will be attached on
##' \code{meta$limits} so that the limits of the axis layer will sync
##' with \code{meta$limits} dynamically.
##' @examples
##' library(cranvas)
##' library(qtbase)
##' library(qtpaint)
##'
##' s = qscene()
##' r = qlayer(s)
##' r[1, 1] = qlayer(paintFun = function(layer, painter) {
##' qdrawCircle(painter, runif(1000, 0, 10), runif(1000, -5, 3), r = 2)
##' qdrawRect(painter, 0, 10, -5, 3)
##' }, limits = qrect(c(0, 10), c(-5, 3))) # main layer
##' ## note the vertical limits of x-axis and horizontal limits of y-axis are [0, 1]
##' r[2, 1] = qaxis(side = 1, at = c(0, 1, 3, 7, 8), limits = qrect(c(0, 10), c(0, 1))) # x-axis
##' r[1, 0] = qaxis(side = 2, at = c(-4.5, -1, 0, 1.5, 2.5), limits = qrect(c(0, 1), c(-5, 3))) # y-axis
##' r[0, 1] = qaxis(side = 3, meta = list(xat = c(1, 3, 7), xlabels = c('a', 'b', 'c'), limits = matrix(c(0, 10, -5, 3), 2))) # top x-axis
##' print(qplotView(scene = s)) # default layout is ugly; tune in r$gridLayout()
##'
qaxis = function(parent = NULL, meta = NULL, side = 1, at = NULL, labels = NULL, ...) {
    gen_limits = function () {
        lims = as.matrix(meta$limits)
        qrect(if (side%%2) cbind(lims[, 1], 0:1) else cbind(0:1, lims[, 2]))
    }
    draw_axis = function(layer, painter) {
        if (is.null(at)) {
            at = if (side%%2) meta$xat else meta$yat
        }
        if (is.null(labels)) {
            labels = if (!is.null(meta)) {
                if (side%%2) meta$xlabels else meta$ylabels
            }
            if (is.null(labels)) labels = format(at)
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
        qdrawSegment(painter, xat + xshift1, yat + yshift1, xat + xshift2, yat + yshift2)
    }
    if (!('limits' %in% names(list(...))) && !is.null(meta)) {
        l = qlayer(parent, paintFun = draw_axis, limits = gen_limits(), ...)
        if (is.environment(meta))
            meta$limitsChanged$connect(function() l$setLimits(gen_limits()))
        l
    } else qlayer(parent, paintFun = draw_axis, ...)
}

##' Calculate pretty locations of axis tick marks.
##' The pretty locations are calculated by the function
##' \code{\link[base]{pretty}}, but the locations that exceed the
##' range of the data are removed. A special case is the factor: the
##' locations are 1, 2, ..., up to the number of levels.
##'
##' @param x the data vector (either numeric or factor)
##' @return a numeric vector of the axis tick marks locations
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @seealso \code{\link[base]{pretty}}
##' @examples library(cranvas)
##' axis_loc(1)
##' axis_loc(c(0, 1))
##' axis_loc(rnorm(100))
##' axis_loc(iris$Species)  # a factor, so locations are 1, 2, 3
##'
axis_loc = function(x) {
    if (is.factor(x)) {
        return(seq_along(levels(x)))
    }
    at = pretty(x)
    at[at <= max(x) & at >= min(x)]
}

##' Create a background grid layer.
##' A layer with gray background and white grid lines corresponding to
##' axis tick marks. Minor grid lines are optional and thinner.
##'
##' @param parent the parent layer (default to be \code{NULL}, which
##' means creating an independent layer with no parents, but it can be
##' added to a parent layer using the approach \code{parent[i, j] <-
##' child_layer})
##' @param meta \code{NULL} means to use \code{xat}, \code{yat},
##' otherwise it should be an object containing child elements
##' \code{xat} and \code{yat}, and it will override the next two
##' arguments; besides, the limits of the layer will be set to
##' \code{meta$limits} so this element should be present as well
##' @param xat locations to draw vertical grid lines
##' @param yat locations to draw horizontal grid lines
##' @param xlim the x-axis limits (\code{c(x0, x1)})
##' @param ylim the y-axis limits (\code{c(y0, y1)})
##' @param minor defines which minor lines to draw: \code{'x'}: only
##' on the x-axis; \code{'y'}: only on the y-axis; \code{'xy'}: both x
##' and y minor grid lines; \code{''}: no minor grid lines
##' @param ... other arguments passed to \code{\link[qtpaint]{qlayer}}
##' @return a layer object
##' @author Yihui Xie <\url{http://yihui.name}>
##' @note If \code{meta} is not \code{NULL}, it is supposed to be a
##' reference object, and an event will be attached on
##' \code{meta$limits} so that the limits of the grid layer will sync
##' with \code{meta$limits} dynamically.
##' @export
##' @seealso \code{\link[graphics]{grid}}, \code{\link[qtpaint]{qlayer}}
##' @examples
##' library(cranvas)
##' library(qtbase)
##' library(qtpaint)
##'
##' s = qscene()
##' r = qlayer(s)
##' l = qrect(matrix(c(0, 1, 0, 1), 2))
##' m = qlayer(paintFun = function(layer, painter) {
##'     qdrawCircle(painter, runif(1000), runif(1000), r = 2)
##'     qdrawRect(painter, 0, 0, 1, 1)
##' }, limits = l) # main layer
##' g = qgrid(xat = seq(0, 1, .2), yat = seq(0, 1, .5), xlim = c(0, 1), ylim = c(0, 1), limits = l)
##' r[1, 1] = g  # must add the grid layer FIRST, then the plot layer
##' r[1, 1] = m
##' print(qplotView(scene = s))
##'
qgrid = function(parent = NULL, meta = NULL, xat, yat, xlim, ylim, minor = 'xy', ...) {
    .bgcolor = "grey90"  # background color
    minor_at = function(at, lim) {
        n = length(at)
        if (n <= 1) return(NULL)
        l = at[1] - at[2]; r = at[n] - at[n - 1]
        at = (at[-1] + at[-n])/2
        n = n - 1
        at = sort(c(seq(at[1], lim[1], l), at[-c(1, n)], seq(at[n], lim[2], r)))
        at[at < lim[2] & at > lim[1]]
    }
    draw_grid = function(layer, painter) {
        if (!is.null(meta)) {
            lims = meta$limits
            xlim = lims[, 1]; ylim = lims[, 2]
            xat = meta$xat; yat = meta$yat
        }
        qdrawRect(painter, xlim[1], ylim[1], xlim[2], ylim[2], stroke = .bgcolor,
            fill = .bgcolor)
        qlineWidth(painter) = 2
        qdrawSegment(painter, xat, ylim[1], xat, ylim[2], stroke = "white")
        qdrawSegment(painter, xlim[1], yat, xlim[2], yat, stroke = "white")
        ## minor grid
        qlineWidth(painter) = 1
        if (minor %in% c('x', 'xy')) {
            xat = minor_at(xat, xlim)
            if (length(xat))
                qdrawSegment(painter, xat, ylim[1], xat, ylim[2], stroke = "white")
        }
        if (minor %in% c('y', 'xy')) {
            yat = minor_at(yat, ylim)
            if (length(yat))
                qdrawSegment(painter, xlim[1], yat, xlim[2], yat, stroke = "white")
        }
    }
    if (!('limits' %in% names(list(...))) && !is.null(meta)) {
        l = qlayer(parent, paintFun = draw_grid, limits = qrect(meta$limits), ...)
        if (is.environment(meta))
            meta$limitsChanged$connect(function() l$setLimits(qrect(meta$limits)))
        l
    } else qlayer(parent, paintFun = draw_grid, ...)
}

##' Create a margin text layer.
##' This function is similar to \code{\link[graphics]{mtext}}, which
##' draws text into the margin of a plot. A slight difference is this
##' function creates a layer which can be put anywhere in the layout.
##' We can also create a title layer with this function.
##'
##' As in R base graphics, the margin means the bottom, left, top and
##' right area of the main plot region. This function will adjust the
##' direction of the text according to the side to which it is drawn,
##' e.g., the left or right side will make the text vertical.
##' @param parent the parent layer (default to be \code{NULL}, which
##' means creating an independent layer with no parents, but it can be
##' added to a parent layer using the approach \code{parent[i, j] <-
##' child_layer})
##' @param meta \code{NULL} means to use \code{text} directly,
##' otherwise it can override \code{text}
##' @param side which side to draw the text (following the convention
##' of R base graphics); e.g. \code{side = 3} can be used to create
##' the title layer
##' @param text the character string to draw; if \code{meta} is not
##' \code{NULL}, this argument will take values from \code{meta$xlab}
##' if \code{side == 1}, \code{meta$ylab} if \code{side == 2}, and
##' \code{meta$main} if \code{side == 3}
##' @param x the x coordinate
##' @param y the y coordinate
##' @param cex the expansion factor
##' @param ... other arguments passed to \code{\link[qtpaint]{qlayer}}
##' @return a layer object
##' @author Yihui Xie <\url{http://yihui.name}>
##' @note The limits of the text layer is [0, 1] both horizontally and
##' vertically by default.
##' @export
##' @seealso \code{\link[graphics]{mtext}}, \code{\link[qtpaint]{qlayer}}
##' @examples library(cranvas)
##' library(qtbase)
##' library(qtpaint)
##'
##' s = qscene()
##' r = qlayer(s)  # root layer
##' m = qlayer(paintFun = function(layer, painter) {
##'     qdrawCircle(painter, runif(1000), runif(1000), r = 2)
##'     qdrawRect(painter, 0, 0, 1, 1)
##' }, limits = qrect(matrix(c(0, 1, 0, 1), 2))) # main layer
##' m1 = qmtext(text = 'x axis title!', side = 1)
##' m2 = qmtext(text = 'y axis title!', side = 2)
##' m3 = qmtext(text = 'THE MAIN TITLE', side = 3)
##' m4 = qmtext(text = 'text on the right margin', side = 4)
##' ## note how to arrange these layers appropriately in the margin
##' r[1, 1] = m
##' r[2, 1] = m1
##' r[1, 0] = m2
##' r[0, 1] = m3
##' r[1, 2] = m4
##' print(qplotView(scene = s))
##'
qmtext = function(parent = NULL, meta = NULL, side = 1, text = '', x = 0.5, y = 0.5,
                  cex = 1, ...) {
    draw_text = function(layer, painter) {
        if (!is.null(meta)) {
            if (side == 1) text = meta$xlab
            if (side == 2) text = meta$ylab
            if (side == 3) text = meta$main
        }
        qdrawText(painter, text, x, y, rot = c(0, 90, 0, 90)[side], cex = cex)
    }
    if (!('limits' %in% names(list(...))))
        qlayer(parent, paintFun = draw_text, limits = qrect(c(0, 1), c(0, 1)), ...) else
    qlayer(parent, paintFun = draw_text, ...)
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
    qdrawRect(plotObj, dataRanges[1], dataRanges[3], dataRanges[2],
        dataRanges[4], fill = "grey80", stroke = "grey80")

    #vertical
    if (!is.null(vertPos)) {
        nvlines <- length(vertPos)
        vdiff <- vertPos[nvlines] - vertPos[nvlines-1]
        qlineWidth(plotObj) <- 2

        qdrawLine(plotObj, x = rep(c(dataRanges[1:2], NA), nvlines),
            y = rep(vertPos, each = 3), stroke = "white")

        minor.vertPos <- c(vertPos[1]-vdiff/2, vertPos+vdiff/2)

        n <- length(minor.vertPos)
        if (dataRanges[4] < minor.vertPos[n])
        	minor.vertPos <- minor.vertPos[-n]

        if (dataRanges[3] > minor.vertPos[1])
        	minor.vertPos <- minor.vertPos[-1]
    }

    #horizontal
    if (!is.null(horiPos)) {
        nhlines <- length(horiPos)
        hdiff <- horiPos[nhlines] - horiPos[nhlines-1]
        qlineWidth(plotObj) <- 2

        qdrawLine(plotObj, x = rep(horiPos, each = 3),
            y = rep(c(dataRanges[3:4], NA), length(horiPos)), stroke = "white")

        minor.horiPos <- c(horiPos[1]-hdiff/2, horiPos+hdiff/2)

        n <- length(minor.horiPos)
        if (dataRanges[2] < minor.horiPos[n]) minor.horiPos <- minor.horiPos[-n]
        if (dataRanges[1] > minor.horiPos[1]) minor.horiPos <- minor.horiPos[-1]
    }

    #minor horizontal
    if (!is.null(minor.vertPos)) {
        # change linewidth to smaller width
        qlineWidth(plotObj) <- 0.1
        qdrawLine(plotObj, x = rep(c(dataRanges[1:2], NA),
            length(minor.vertPos)), y = rep(minor.vertPos, each = 3),
            stroke = "white")

    }

    #minor vertical
    if (!is.null(minor.horiPos)) {

        # change linewidth to smaller width
        qlineWidth(plotObj) <- 0.1
        qdrawLine(plotObj, x = rep(minor.horiPos, each = 3),
            y = rep(c(dataRanges[3:4], NA), length(minor.horiPos)),
            stroke = "white")
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

