##' Boxplots for variables in the data or a continuous variable vs a
##' categorical variable
##'
##' This function can draw side-by-side boxplots for all the variables
##' in a data frame or boxplots for a continous variable vs a
##' categorical variable.
##'
##' The boxplots can respond to changes in the brushed rows, i.e., in
##' \code{selected(data)}. When we brush in other plots which are
##' based on the same data, there will be ``child'' boxplots in this
##' plot showing the distributions of the brushed data.
##' @param vars a list of variables (a character vector), or a
##' formula; a one-sided formula like \code{~ x1 + x2 + x3} means to
##' draw side-by-side boxplots for the variables in the right hand
##' side, whereas a two-sided formula like \code{y ~ x} means boxplots
##' of a continuous \code{y} against a categorical \code{x}
##' @inheritParams qbar
##' @param at the locations of the boxplots (by default from 1 to
##' \code{p} where \code{p} is the number of variables to plot or the
##' number of levels of the categorical variable)
##' @param width width(s) of boxes (do not have to be a same value if
##' provided as a numeric vector); by default it is about 1/10 of the
##' screen width
##' @param horizontal horizontal or vertical boxplots
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @example inst/examples/qboxplot-ex.R
##' @export
qboxplot = function(vars, data = last_data(), at = NULL, width = NULL, horizontal = FALSE) {
    data = check_data(data)
    if (missing(vars)) vars = grep('^[^.]', names(data), value = TRUE)
    if (inherits(vars, 'formula')) {
        vars.n = length(vars)  # 2 means one-sided formula, 3 means two-sided
        vars.a = all.vars(vars)  # all variables in the formula
        if (vars.n == 2) {
            vars = all.vars(vars)
            y = as.matrix(data[, vars])
            xlabels = vars
        } else if (vars.n == 3) {
            r = range(data[, vars.a[1]], na.rm = TRUE)
            p = length(unique(data[, vars.a[2]]))
            y = as.vector(data[, vars.a[1]])
            xlabels = levels(factor(data[, vars.a[2]]))
        }
    } else {
        r = range(if (is.mutaframe(data)) as.data.frame(data)[, vars] else data[, vars],
                  na.rm = TRUE)
        p = length(vars)
        y = as.matrix(as.data.frame(data[, vars]))
        xlabels = vars
    }
    #data = data[, vars, drop = FALSE]
    if (is.null(at)) at = 1:p
    if (is.null(width)) width = max(0.1 * diff(range(at)), 0.2)
    lims = matrix(c(range(at) + c(-1, 1) * max(width)/2, r), 2)
    ext = matrix(c(0.1, 0.1, 0.05, 0.05), 2)
    if (horizontal) {
        lims = lims[, 2:1]
        ext = ext[, 2:1]
    }
    lims = extend_ranges(lims, ext)  # extend the limits here
    layer.main = qbxp(vars = vars, data = data, at = at, width = width,
                            horizontal = horizontal, limits = qrect(lims))
    xat = at
    yat = axis_loc(y)
    ylabels = format(yat)
    if (horizontal) {
        tmp = xat; xat = yat; yat = tmp
        tmp = xlabels; xlabels = ylabels; ylabels = tmp
    }

    scene = qscene()
    layer.root = qlayer(scene)
    layer.xaxis = qaxis(at = xat, labels = xlabels, side = 1, sister = layer.main)
    layer.yaxis = qaxis(at = yat, labels = ylabels, side = 2, sister = layer.main)
    layer.grid = qgrid(xat = xat, yat = yat, sister = layer.main,
                       minor = ifelse(horizontal, 'x', 'y'))
    layer.brush = qbxp(vars = vars, data = data, at = at, width = width,
                             subset = TRUE, horizontal = horizontal, sister = layer.main)
    layer.root[1, 1] = layer.grid
    layer.root[1, 1] = layer.main
    layer.root[1, 1] = layer.brush
    layer.root[1, 0] = layer.yaxis
    layer.root[2, 1] = layer.xaxis
    layer.root[1, 2] = qlayer()  # place-holder

    layout = layer.root$gridLayout()
    layout$setRowPreferredHeight(0, 30)
    layout$setColumnPreferredWidth(0, prefer_width(ylabels))
    layout$setRowPreferredHeight(2, prefer_height(xlabels))
    layout$setColumnMaximumWidth(2, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setColumnStretchFactor(0, 0)
    layout$setRowStretchFactor(2, 0)

    if (is.mutaframe(data)) {
        add_listener(data, function(i, j) {
            ## layer.main$setLimits(qrect(matrix(c(0, 3, -3, 10), 2)))
            qupdate(layer.main)
        })
    }
    view = qplotView(scene = scene)
    ## view$setWindowTitle(main)
    view
}


##' Create a boxplot layer
##'
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
            .boxcol = 'gray'
            data2 = data[selected(data), , drop = FALSE]
            width = mean(selected(data)) * width
        }
        if (!is.null(vars) && inherits(vars, 'formula') && length(vars) == 3) {
            vars.a = all.vars(vars)
            data2 = tapply(data[, vars.a[1]], data[, vars.a[2]], I, simplify = FALSE)  # reshape
            vars = names(data2)
        } else if (is.character(vars))
            data2 = as.data.frame((if (subset) data2 else data)[, vars, drop = FALSE])
        ## if meta data is passed here, use it
        if (!is.mutaframe(data) && !is.null(data$plot.data) && !is.null(data$numeric.col)) {
            data2 = as.data.frame(data$plot.data)[, data$numeric.col, drop = FALSE]
            at = which(data$numeric.col)
        }
        ## boxplots statistics
        bxp.data = sapply(data2, boxplot.stats, do.conf = FALSE, simplify = FALSE)
        bxp.stats = sapply(bxp.data, `[[`, 'stats')  # quantiles
        bxp.out = sapply(bxp.data, `[[`, 'out', simplify = FALSE)  # outliers
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
        qdrawRect(painter, x0, y0, x1, y1, fill = ifelse(subset, '#FFFF0099', 'white'),
                  stroke = .boxcol)  # box
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
