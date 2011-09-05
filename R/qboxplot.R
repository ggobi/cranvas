##' Draw boxplots for several variables in the data or a continuous
##' variable vs a categorical variable
##'
##' This function can draw side-by-side boxplots for all the variables
##' in a data frame or boxplots for a continous variable vs a
##' categorical variable.
##'
##' Common interactions are documented in
##' \code{\link{common_key_press}}. Note boxplots  also supports
##' brushing and can respond to brushing in other plots. When we brush
##' in other plots which are based on the same data, there will be
##' ``child'' boxplots in this plot showing the distributions of the
##' brushed data.
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
##' @param points whether to add data points to the boxplot
##' @return A boxplot
##' @author Yihui Xie <\url{http://yihui.name}>
##' @example inst/examples/qboxplot-ex.R
##' @export
##' @family plots
qboxplot =
    function(vars, data = last_data(), at = NULL, width = NULL, horizontal = FALSE,
             main = '', xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL,
             points = FALSE) {
    data = check_data(data)
    b = brush(data)
    meta = Box.meta$new(horizontal = horizontal, main = main, alpha = 1, points = points)
    if (missing(vars)) vars = grep('^[^.]', names(data), value = TRUE)

    compute_coords = function(brush = FALSE) {
        meta$minor = ifelse(meta$horizontal, 'x', 'y')
        idx = visible(data)
        if (brush) idx = idx & selected(data)
        if (inherits(vars, 'formula')) {
            vars.n = length(vars)  # 2 means one-sided formula, 3 means two-sided
            vars.a = all.vars(vars)     # all variables in the formula
            if (vars.n == 2) {
                meta$vars = all.vars(vars)
                if (identical(meta$vars, '.'))
                    meta$vars = grep('^[^.]', names(data), value = TRUE)
                ylist = lapply(as.data.frame(data[idx, meta$vars, drop = FALSE]), as.numeric)
                if (!brush) {
                    meta$xlab = if (is.null(xlab)) 'variable' else xlab
                    meta$ylab = if (is.null(ylab)) 'value' else ylab
                }
            } else if (vars.n == 3) {
                meta$xvar = vars.a[2]; meta$yvar = vars.a[1]
                ylist = split(data[idx, meta$yvar], data[idx, meta$xvar])
                if (!brush) {
                    meta$xlab = if (is.null(xlab)) vars.a[2] else xlab
                    meta$ylab = if (is.null(ylab)) vars.a[1] else ylab
                }
            }
        } else {
            ylist = lapply(as.data.frame(data[idx, vars, drop = FALSE]), as.numeric)
            if (!brush) {
                meta$vars = names(data[, vars, drop = FALSE])
                meta$xlab = if (is.null(xlab)) 'variable' else xlab
                meta$ylab = if (is.null(ylab)) 'value' else ylab
            }
        }
        bxp.data = lapply(ylist, boxplot.stats, do.conf = FALSE)
        bxp.stats = sapply(bxp.data, `[[`, 'stats')  # quantiles
        bxp.out = lapply(bxp.data, `[[`, 'out')  # outliers
        if (brush) {
            meta$bxp.stats2 = bxp.stats
            return()
        }

        meta$bxp.stats = bxp.stats; meta$bxp.out = bxp.out
        meta$xlabels = if (length(meta$vars)) meta$vars else names(ylist)
        meta$yat = axis_loc(range(ylist)); meta$ylabels = format(meta$yat)
        meta$xat = meta$at = if (is.null(at)) seq_along(meta$xlabels) else at
        meta$width = if (is.null(width)) max(0.2 * diff(range(meta$at)), 0.3) else width
        meta$limits =
            cbind(extend_ranges(if (is.null(xlim))
                                range(meta$xat) + c(-1, 1) * max(meta$width)/2 else xlim,
                                ifelse(length(meta$xat) == 1, 5, 2) * qpar('mar')),
                  extend_ranges(if (is.null(ylim)) range(ylist) else ylim))
        if (length(meta$vars)) {
            meta$y =
                c(vapply(as.data.frame(data[, meta$vars, drop = FALSE]), as.numeric,
                         numeric(nrow(data))))
            meta$x = rep(meta$at, each = nrow(data))
        } else {
            meta$y = data[, meta$yvar]; meta$x = meta$at[as.integer(data[, meta$xvar])]
        }
    }
    compute_coords()
    compute_colors = function() {
        if (!meta$points) {
            meta$color = NA; meta$border = NA
        } else {
            if (length(meta$vars)) {
                idx = !visible(data)
                meta$color = data$.color; meta$border = data$.border
                meta$color[idx] = NA; meta$border[idx] = NA
            } else {
                meta$color = meta$border = 'gray15'
            }
        }
    }
    compute_colors()
    flip_coords = function() {
        if (!meta$horizontal) return()
        switch_value('x', 'y', meta)
        switch_value('xat', 'yat', meta)
        switch_value('xlabels', 'ylabels', meta)
        switch_value('xlab', 'ylab', meta)
        meta$limits = meta$limits[, 2:1]
    }
    flip_coords()
    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15

    main_draw = function(layer, painter) {
        qdrawGlyph(painter, qglyphCircle(r = data$.size[1]), meta$x, meta$y,
                   stroke = meta$border, fill = meta$color)
    }

    brush_mouse_press = function(layer, event) {
        common_mouse_press(layer, event, data, meta)
    }
    brush_mouse_move = function(layer, event) {
        rect = qrect(update_brush_size(meta, event))
        hits = layer$locate(rect)
        if (length(hits)) {
            if (length(meta$vars))
                hits = hits %% nrow(data)
            hits = hits + 1
        }
        selected(data) = mode_selection(selected(data), hits, mode = b$mode)
        common_mouse_move(layer, event, data, meta)
    }
    brush_mouse_release = function(layer, event) {
        brush_mouse_move(layer, event)
        common_mouse_release(layer, event, data, meta)
    }
    key_press = function(layer, event) {
        common_key_press(layer, event, data, meta)
    }
    key_release = function(layer, event) {
        common_key_release(layer, event, data, meta)
    }

    scene = qscene()
    layer.bxp = qbxp(data, meta, limits = qrect(meta$limits))
    layer.main =
        qlayer(paintFun = main_draw, mousePressFun = brush_mouse_press,
               mouseReleaseFun = brush_mouse_release,
               mouseMove = brush_mouse_move,
               keyPressFun = key_press, keyReleaseFun = key_release,
               limits = qrect(meta$limits), clip = TRUE)

    layer.root = qlayer(scene)
    layer.brush = qbxp(data, meta, subset = TRUE, limits = qrect(meta$limits))
    layer.title = qmtext(meta = meta, side = 3)
    layer.xlab = qmtext(meta = meta, side = 1)
    layer.ylab = qmtext(meta = meta, side = 2)
    layer.xaxis = qaxis(meta = meta, side = 1)
    layer.yaxis = qaxis(meta = meta, side = 2)
    layer.grid = qgrid(meta = meta)
    layer.root[0, 2] = layer.title
    layer.root[2, 2] = layer.xaxis
    layer.root[3, 2] = layer.xlab
    layer.root[1, 1] = layer.yaxis
    layer.root[1, 0] = layer.ylab
    layer.root[1, 2] = layer.grid
    layer.root[1, 2] = layer.bxp
    layer.root[1, 2] = layer.main
    layer.root[1, 2] = layer.brush
    layer.root[1, 3] = qlayer()

    set_layout = function() {
        fix_dimension(layer.root,
                      row = list(id = c(0, 2, 3), value = c(prefer_height(meta$main),
                                                  prefer_height(meta$xlabels),
                                                  prefer_height(meta$xlab))),
                      column = list(id = c(1, 0, 3), value = c(prefer_width(meta$ylabels),
                                                     prefer_width(meta$ylab, FALSE),
                                                     10)))
    }
    set_layout()
    meta$mainChanged$connect(set_layout)
    meta$xlabChanged$connect(set_layout); meta$ylabChanged$connect(set_layout)
    meta$xlabelsChanged$connect(set_layout); meta$ylabelsChanged$connect(set_layout)

    view = qplotView(scene = scene)
    view$setWindowTitle(paste("Boxplot:", if (length(meta$vars))
                              paste(meta$vars, collapse = ', ') else
                              paste(meta$yvar, meta$xvar, sep = ' ~ ')))

    d.idx = add_listener(data, function(i, j) {
        idx = which(j == c('.brushed', '.color', '.border'))
        if (length(idx) < 1) {
            compute_coords(); compute_colors()
            qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
            layer.main$invalidateIndex(); qupdate(layer.main)
            return()
        } else idx = c(1, 2, 2)[idx]
        switch(idx, {compute_coords(brush = TRUE); qupdate(layer.brush)},
           {compute_color(); qupdate(layer.main)})
    })
    qconnect(layer.main, 'destroyed', function(x) {
        remove_listener(data, d.idx)
    })

    b$cursorChanged$connect(function() {
        set_cursor(view, b$cursor)
    })
    sync_limits(meta, layer.main, layer.brush, layer.bxp)  # sync limits
    meta$manual.brush = function(pos) {
        brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
    }

    attr(view, 'meta') = meta
    view
}

Box.meta =
    setRefClass("Box_meta", fields = signalingFields(c(

                            Common.meta,

                            list(vars = 'character', x = 'numeric', y = 'numeric',
                                 xvar = 'character', yvar = 'character',
                                 at = 'numeric', width = 'numeric', horizontal = 'logical',
                                 bxp.stats = 'matrix', bxp.out = 'list', points = 'logical',
                                 bxp.stats2 = 'matrix')

                            )))

##' Create a boxplot layer
##'
##' A ``low-level'' plotting function to create a boxplot layer.
##'
##' @inheritParams qbar
##' @param meta the meta data
##' @param subset whether to draw boxplots based on selected rows
##' @param ... other arguments passed to \code{\link[qtpaint]{qlayer}}
##' @return a layer object
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples ## see source code of qboxplot()
qbxp = function(data, meta, subset = FALSE, ...) {
    draw_boxplot = function(layer, painter) {
        .boxcol = 'black'
        width = meta$width
        if (subset) {
            bxp.stats = meta$bxp.stats2
            if (!nrow(bxp.stats)) return()
            .boxcol = 'gray'
            width = mean(selected(data)) * width
        } else bxp.stats = meta$bxp.stats
        if (!subset) bxp.out = meta$bxp.out
        at = meta$at; horizontal = meta$horizontal
        x0 = rep(at, each = 2); y0 = as.vector(bxp.stats[c(1, 4), ])
        x1 = x0; y1 = as.vector(bxp.stats[c(2, 5), ])
        if (horizontal) {
            switch_value('x0', 'y0', sys.frame(1))
            switch_value('x1', 'y1', sys.frame(1))
        }
        qdrawSegment(painter, x0, y0, x1, y1, stroke = .boxcol)  # whiskers

        x0 = at - width/2; x1 = at + width/2
        y0 = bxp.stats[2, ]; y1 = bxp.stats[4, ]
        if (horizontal) {
            switch_value('x0', 'y0', sys.frame(1))
            switch_value('x1', 'y1', sys.frame(1))
        }
        qdrawRect(painter, x0, y0, x1, y1, fill = ifelse(subset, '#FFFF0099', 'white'),
                  stroke = .boxcol)  # box

        if (!subset && length(bxp.out)) {
            y = unlist(bxp.out); x = rep(at, sapply(bxp.out, length))
            if (horizontal) {
                switch_value('x', 'y', sys.frame(1))
            }
            circle = qglyphCircle(r = data$.size[1])
            qdrawGlyph(painter, circle, x, y, stroke = 'black', fill = 'black')
        }

        qlineWidth(painter) = 3
        if (horizontal) {
            x0 = x1 = bxp.stats[3, ]
        } else {
            y0 = y1 = bxp.stats[3, ]
        }
        qdrawSegment(painter, x0, y0, x1, y1, stroke = .boxcol)  # median bar
        qlineWidth(painter) = 1

        if (subset) draw_brush(layer, painter, data, meta)
    }
    qlayer(paintFun = draw_boxplot, ...)
}
