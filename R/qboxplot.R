##' Boxplots
##'
##'
##' @param vars a list of variables, or a formula
##' @param data a mutaframe
##' @param at the locations of the boxplots
##' @param width width(s) of boxes
##' @param horizontal horizontal or vertical boxplots
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' df = matrix(rnorm(1000), 100)
##' qboxplot(data = df)
##' qboxplot(data = df, horizontal = TRUE)
##'
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
if (FALSE) {
    df = matrix(rnorm(1000), 200)
    qboxplot(df)
    qboxplot(df, horizontal = TRUE)
    qboxplot(df, at = (1:5)^2)  # at different locations
    qboxplot(df, width = .1*sample(5))  # different widths
    qboxplot(rnorm(100))
    qboxplot(Sepal.Length~Species,data=iris)
    df = qdata(data.frame(x = rnorm(100), y = runif(100)))
    qboxplot(c('x', 'y'), df)
}

## construct the boxplot layer
.bxp.layer = function(parent = NULL, vars, data, subset = FALSE, at, width, horizontal, ...) {
    draw_boxplot = function(layer, painter) {
        .boxcol = 'black'
        if (subset) {
            if (all(!selected(data))) return()
            .boxcol = brush(data)$color
            data = data[selected(data), ]
        }
        if (is(vars, 'formula') && length(vars) == 3) {
            vars.a = all.vars(vars)
            data = tapply(data[, vars.a[1]], data[, vars.a[2]], I, simplify = FALSE)  # reshape
            vars = names(data)
        }
        if (is.mutaframe(data)) data = as.data.frame(data[, vars])
        ## boxplots statistics
        bxp.data = sapply(data, boxplot.stats, do.conf = FALSE, simplify = FALSE)
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
        ## qlineWidth(painter) = 3
        if (horizontal) {
            x0 = x1 = bxp.stats[3, ]
        } else {
            y0 = y1 = bxp.stats[3, ]
        }
        qdrawSegment(painter, x0, y0, x1, y1, stroke = .boxcol)  # median bar
        ## qlineWidth(painter) = 1
    }
    qlayer(parent, paintFun = draw_boxplot, ...)
}
