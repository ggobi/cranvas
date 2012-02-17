##' Create an axis layer
##'
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
##' @example inst/examples/qaxis-ex.R
qaxis = function(parent = NULL, meta = NULL, side = 1, at = NULL, labels = NULL, ...) {
    xside = side%%2
    gen_limits = function () {
        lims = as.matrix(meta$limits)
        qrect(if (xside) cbind(lims[, 1], 0:1) else cbind(0:1, lims[, 2]))
    }
    draw_axis = function(layer, painter) {
        if (is.null(at)) {
            at = if (xside) meta$xat else meta$yat
        }
        if (is.null(labels)) {
            labels = if (!is.null(meta)) {
                if (xside) meta$xlabels else meta$ylabels
            }
            if (is.null(labels)) labels = format(at)
        }
        if (!is.null(meta)) {
            r = sort(meta$limits[, c(2, 1)[xside + 1]])
            idx = (at > r[1]) & (at < r[2])  # censor locations out of limits
            at = at[idx]; labels = labels[idx]
        }
        if (!length(at)) return()
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

##' Calculate pretty locations of axis tick marks
##'
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
    at[at <= max(x, na.rm = TRUE) & at >= min(x, na.rm = TRUE)]
}

##' Create a background grid layer
##'
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
    .bgcolor = "gray90"  # background color
    minor_at = function(at, lim) {
        n = length(at)
        if (n <= 1) return(NULL)
        l = at[1] - at[2]; r = at[n] - at[n - 1]
        at = (at[-1] + at[-n])/2
        n = n - 1
        if (at[n] < lim[2])
            at = c(at, seq(at[n], lim[2], r/2)[-1])
        if (at[1] > lim[1])
            at = c(seq(at[1], lim[1], l/2)[-1], at)
        at[at < lim[2] & at > lim[1]]
    }
    major_at = function(at, lim) {
        if ((n <- length(at)) <= 1) return(at)
        l = at[1] - at[2]; r = at[n] - at[n - 1]
        if (at[n] < lim[2])
            at = c(at, seq(at[n], lim[2], r)[-1])
        if (at[1] > lim[1])
            at = c(rev(seq(at[1], lim[1], l)[-1]), at)
        at[at < lim[2] & at > lim[1]]
    }
    draw_grid = function(layer, painter) {
        if (!is.null(meta)) {
            lims = meta$limits
            xlim = lims[, 1]; ylim = lims[, 2]
            xat = meta$xat; yat = meta$yat; minor = meta$minor
        }
        qdrawRect(painter, xlim[1], ylim[1], xlim[2], ylim[2], stroke = .bgcolor,
            fill = .bgcolor)
        qlineWidth(painter) = 2
        xat = major_at(xat, xlim); yat = major_at(yat, ylim)
        qdrawSegment(painter, xat, ylim[1], xat, ylim[2], stroke = "white")
        qdrawSegment(painter, xlim[1], yat, xlim[2], yat, stroke = "white")
        ## minor grid
        qlineWidth(painter) = 1
        if (minor %in% c('x', 'xy')) {
            xat = minor_at(xat, xlim)
            if (length(xat))
                qdrawSegment(painter, xat, ylim[1], xat, ylim[2], stroke = "grey95")
        }
        if (minor %in% c('y', 'xy')) {
            yat = minor_at(yat, ylim)
            if (length(yat))
                qdrawSegment(painter, xlim[1], yat, xlim[2], yat, stroke = "grey95")
        }
    }
    if (!('limits' %in% names(list(...))) && !is.null(meta)) {
        l = qlayer(parent, paintFun = draw_grid, limits = qrect(meta$limits), ...)
        if (is.environment(meta))
            meta$limitsChanged$connect(function() l$setLimits(qrect(meta$limits)))
        l
    } else qlayer(parent, paintFun = draw_grid, ...)
}

##' Create a margin text layer
##'
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
