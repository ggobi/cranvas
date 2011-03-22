qaxis = function(parent = NULL, data, side = 1, at = NULL, labels = NULL, limits, ...) {
    lims = if(side %% 2) qrect(limits, c(0, 1)) else qrect(c(0, 1), limits)
    draw_axis = function(layer, painter) {
        if (is.null(at)) {
            at = .axis.loc(data)
        }
        if (is.null(labels)) {
            labels = if (is.factor(data)) levels(data) else format(at)
        }
        xat = yat = at; xalign = yalign = 'center'
        xshift1 = yshift1 = xshift2 = yshift2 = 0
        ## side = 1, 2, 3, 4
        switch(side, {
            yat = 0.9
            yalign = 'top'
            yshift1 = 0.01
            yshift2 = 0.1
        }, {
            xat = 0.9
            xalign = 'right'
            xshift1 = 0.01
            xshift2 = 0.1
        }, {
            yat = 0.1
            yalign = 'bottom'
            yshift1 = -0.01
            yshift2 = -0.1
        }, {
            xat = 0.1
            xalign = 'left'
            xshift1 = -0.01
            xshift2 = -0.1
        })
        qdrawText(painter, labels, x = xat, y = yat, halign = xalign, valign = yalign)
        qdrawSegment(painter, xat + xshift1, yat + yshift1, xat + xshift2, yat + yshift2)
    }
    qlayer(parent, paintFun = draw_axis, limits = lims, ...)
}

.axis.loc = function(data) {
    if (is.factor(data)) {
        at = as.integer(data)
    } else {
        at = pretty(data)
    }
    at[at <= max(data) & at >= min(data)]
}

qgrid = function(parent = NULL, xat, yat, xlim, ylim, ...) {
    ## background color
    .bgcolor = "grey80"

    draw_grid = function(layer, painter) {
        qdrawRect(painter, xlim[1], ylim[1], xlim[2], ylim[2], stroke = .bgcolor,
                  fill = .bgcolor)
        qdrawSegment(painter, xat, ylim[1], xat, ylim[2], stroke = "white")
        qdrawSegment(painter, xlim[1], yat, xlim[2], yat, stroke = "white")
    }
    qlayer(parent, paintFun = draw_grid, limits = qrect(xlim, ylim), ...)
}
