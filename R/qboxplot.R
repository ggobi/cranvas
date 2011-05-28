##' Boxplots for a data frame or a continuous variable vs a categorical variable.
##'
##' This function can draw side-by-side boxplots for all the variables
##' in a data frame or boxplots for a continous variable vs a
##' categorical variable.
##'
##' @param vars a list of variables, or a formula
##' @param data a mutaframe
##' @param at the locations of the boxplots
##' @param width width(s) of boxes
##' @param horizontal horizontal or vertical boxplots
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @example cranvas/inst/examples/qboxplot-ex.R
##' @export
qboxplot = function(vars, data, at = NULL, width = NULL, horizontal = FALSE) {
    if (missing(data)) {
        ## if no data is provided, I assume vars is the data
        data = if (is.mutaframe(vars)) vars else as.data.frame(vars)
        vars = names(data)
    }
    if (!any(class(data) %in% c('data.frame', 'mutaframe', 'list')))
        data = as.data.frame(data)
    if (missing(vars)) vars = grep('^[^.]', names(data), value = TRUE)
    if (is(vars, 'formula')) {
        vars.n = length(vars)  # 2 means one-sided formula, 3 means two-sided
        vars.a = all.vars(vars)  # all variables in the formula
        if (vars.n == 2) {
            vars = all.vars(vars)
            y = as.matrix(data[, vars])
            xticklab = vars
        } else if (vars.n == 3) {
            r = range(data[, vars.a[1]], na.rm = TRUE)
            p = length(unique(data[, vars.a[2]]))
            y = as.vector(data[, vars.a[1]])
            xticklab = levels(factor(data[, vars.a[2]]))
        }
    } else {
        r = range(if (is.mutaframe(data)) as.data.frame(data)[, vars] else data[, vars],
                  na.rm = TRUE)
        p = length(vars)
        y = as.matrix(as.data.frame(data[, vars]))
        xticklab = vars
    }
    #data = data[, vars, drop = FALSE]
    if (missing(at)) at = 1:p
    if (missing(width)) width = max(0.1 * diff(range(at)), 0.2)
    lims = matrix(c(range(at) + c(-1, 1) * range(width)/2, r), 2)
    if (horizontal) lims = lims[, 2:1]
    lims = .extend.ranges(lims)  # extend the limits here
    main_layer = .bxp.layer(vars = vars, data = data, at = at, width = width,
                            horizontal = horizontal, limits = qrect(lims))
    xat = at
    yat = .axis.loc(y)
    yticklab = format(yat)
    if (horizontal) {
        tmp = xat; xat = yat; yat = tmp
        tmp = xticklab; xticklab = yticklab; yticklab = tmp
    }

    scene = qscene()
    root_layer = qlayer(scene)
    xaxis_layer = qaxis(at = xat, labels = xticklab, side = 1, limits = lims[1:2])
    yaxis_layer = qaxis(at = yat, labels = yticklab, side = 2, limits = lims[3:4])
    grid_layer = qgrid(xat = xat, yat = yat, xlim = lims[1:2], ylim = lims[3:4])
    brush_layer = .bxp.layer(vars = vars, data = data, at = at, width = .8*width,
                             subset = TRUE, horizontal = horizontal, limits = qrect(lims))
    root_layer[1, 1] = grid_layer
    root_layer[1, 1] = main_layer
    root_layer[1, 1] = brush_layer
    root_layer[1, 0] = yaxis_layer
    root_layer[2, 1] = xaxis_layer
    root_layer[2, 1] = qlayer()  # place-holder

    layout = root_layer$gridLayout()
    layout$setRowPreferredHeight(0, 30)
    ## the y-axis layer needs 'dynamic' width determined by #{characters}
    ## here is a formula by my rule of thumb: 9 * nchar + 5
    layout$setColumnPreferredWidth(0, 9 * max(nchar(unlist(strsplit(yticklab, '\n')))) + 5)
    layout$setRowPreferredHeight(2, 15 * max(sapply(gregexpr('\\n', xticklab),
                              function(xx) ifelse(any(xx <0), 0, length(xx)) + 2)))
    layout$setColumnMaximumWidth(2, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setColumnStretchFactor(0, 0)
    layout$setRowStretchFactor(2, 0)

    if (is.mutaframe(data)) {
        add_listener(data, function(i, j) {
            ## main_layer$setLimits(qrect(matrix(c(0, 3, -3, 10), 2)))
            qupdate(main_layer)
        })
    }
    view = qplotView(scene = scene)
    ## view$setWindowTitle(main)
    view
}


##' Create a boxplot layer.
##' A ``low-level'' plotting function to create a boxplot layer.
##'
##' @param parent the parent layer in which to embed this boxplot
##' layer
##' @param vars see \code{\link{qboxplot}}
##' @param data the data (a data frame or mutaframe or a list
##' containing child elements \code{plot.data} and \code{numeric.col})
##' @param subset whether to draw boxplots based on selected rows
##' @param at the locations at which to draw the boxplots
##' @param width width(s) of boxes
##' @param horizontal direction of boxplots (\code{TRUE} means
##' horizontal)
##' @param sister a sister layer on top of which to put the boxplot
##' layer
##' @param ... other arguments passed to \code{\link[qtpaint]{qlayer}}
##' @return a layer object
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples
##' library(cranvas)
##' library(qtbase)
##' library(qtpaint)
##' library(plumbr)
##'
##' s = qscene()
##' r = qlayer(s)
##' x = as.data.frame(matrix(rnorm(100), 20))
##' m = qlayer(paintFun = function(layer, painter) {
##'     qdrawCircle(painter, as.vector(col(x)), as.vector(as.matrix(x)), r = 2, stroke = 'gray')
##' }, limits = qrect(matrix(c(0, 6, range(x)), 2)))  # main layer
##' b = qbxp(vars = c('V1', 'V3', 'V5'), data = x, at = c(1, 3, 5), width = 1, sister = m)
##' r[1, 1] = b
##' r[1, 1] = m
##' print(qplotView(scene = s))
##'
##' ## when the data argument is a list
##' s = qscene()
##' r = qlayer(s)
##' m = qlayer(paintFun = function(layer, painter) {
##'     qdrawCircle(painter, as.vector(col(x)), as.vector(as.matrix(x)), r = 2, stroke = 'gray')
##' }, limits = qrect(matrix(c(0, 6, range(x)), 2)))  # main layer
##' b = qbxp(data = list(plot.data = x, numeric.col = rep(TRUE, 5)), width = 0.6, sister = m)
##' r[1, 1] = b
##' r[1, 1] = m
##' print(qplotView(scene = s))
##'
qbxp = function(parent = NULL, vars = NULL, data, subset = FALSE, at, width,
                horizontal = FALSE, sister = NULL, ...) {
    draw_boxplot = function(layer, painter) {
        .boxcol = 'black'
        if (subset) {
            if (all(!selected(data))) return()
            .boxcol = brush(data)$color
            data2 = data[selected(data), ]
        }
        if (!is.null(vars) && is(vars, 'formula') && length(vars) == 3) {
            vars.a = all.vars(vars)
            data2 = tapply(data[, vars.a[1]], data[, vars.a[2]], I, simplify = FALSE)  # reshape
            vars = names(data2)
        }
        if (is.mutaframe(data) || is.data.frame(data)) data2 = as.data.frame(data[, vars])
        ## if meta data is passed here, use it
        if (!is.null(data$plot.data) && !is.null(data$numeric.col)) {
            data2 = as.data.frame(data$plot.data)[, data$numeric.col, drop = FALSE]
            at = which(data$numeric.col)
        }
        ## boxplots statistics
        bxp.data = sapply(data2, boxplot.stats, do.conf = FALSE, simplify = FALSE)
        bxp.stats = sapply(bxp.data, `[[`, 'stats')  # quantiles
        bxp.out = sapply(bxp.data, `[[`, 'out')  # outliers
        if (horizontal) {
            y0 = rep(at, each = 2)
            x0 = as.vector(bxp.stats[c(1, 4), ])
            y1 = y0
            x1 = as.vector(bxp.stats[c(2, 5), ])
        } else {
            x0 = rep(at, each = 2)
            y0 = as.vector(bxp.stats[c(1, 4), ])
            x1 = x0
            y1 = as.vector(bxp.stats[c(2, 5), ])
        }
        qdrawSegment(painter, x0, y0, x1, y1, stroke = .boxcol)  # whiskers
        if (horizontal) {
            y0 = at - width/2
            y1 = at + width/2
            x0 = bxp.stats[2, ]
            x1 = bxp.stats[4, ]
        } else {
            x0 = at - width/2
            x1 = at + width/2
            y0 = bxp.stats[2, ]
            y1 = bxp.stats[4, ]
        }
        qdrawRect(painter, x0, y0, x1, y1, fill = 'white', stroke = .boxcol)  # box
        if (horizontal) {
            y = rep(at, sapply(bxp.out, length))
            x = unlist(bxp.out)
        } else {
            x = rep(at, sapply(bxp.out, length))
            y = unlist(bxp.out)
        }
        if (!subset) {
            circle = qglyphCircle()
            qdrawGlyph(painter, circle, x, y, cex = .6, stroke = 'black', fill = 'black')
        }
        qlineWidth(painter) = 3
        if (horizontal) {
            x0 = x1 = bxp.stats[3, ]
        } else {
            y0 = y1 = bxp.stats[3, ]
        }
        qdrawSegment(painter, x0, y0, x1, y1, stroke = .boxcol)  # median bar
        qlineWidth(painter) = 1
    }
    if (!('limits' %in% names(list(...))) && !is.null(sister))
        qlayer(parent, paintFun = draw_boxplot, limits = sister$limits(), ...) else
    qlayer(parent, paintFun = draw_boxplot, ...)
}
