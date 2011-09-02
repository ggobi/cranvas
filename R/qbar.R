##' Draw a bar plot
##'
##' This function creates a bar plot based on a categorical variable
##' to show the counts of all categories. Another categorical variable
##' can be used to further split each bar into sub-categories, which
##' will make the bar plot a representation of a contingency
##' table. The splitting variable can be specified in
##' \code{\link{qdata}} using either the \code{color} argument or the
##' \code{border} argument.
##'
##' All the common interactions like brushing and deleting are
##' documented in \code{\link{common_key_press}}.
##'
##' In the identify mode (press the key \code{?} to toggle between
##' brush and identify mode), the variable and its identified values
##' are shown as a text label in the plot, along with the counts and
##' proportion of the identified categories. If the bar plot is split
##' by an additional categorical variable, it will also be shown in
##' the label.
##'
##' A zero-count category is represented by a one-pixel rectangle,
##' which is a useful visual hint to indicate the presence of this
##' category.
##'
##' The x-axis (or y-axis when \code{horizontal = TRUE}) tickmark
##' locations are from 1 to \code{n} shifted to the right by 0.5
##' (i.e. 1.5, 2.5, ...), where \code{n} is the number of levels of
##' the factor variable to be plotted.
##' @param x a variable name (will be coerced to a factor if it is
##' not; \code{NA} will also be a level of the factor if the variable
##' has any \code{NA}'s)
##' @param data a mutaframe created by \code{\link{qdata}} (default to
##' be \code{\link{last_data}()}, i.e. the lastly used data)
##' @param space the space between bars proportional to the width of
##' bars
##' @param main the main title
##' @param horizontal \code{TRUE} to draw a horizontal plot or
##' \code{FALSE} (vertical)
##' @param standardize logical: whether to standardize the height of
##' each bar to 1
##' @param xlim a numeric vector of length 2 (like \code{c(x0, x1)})
##' for x-axis limits; it will be calculated from the data limits if
##' not specified (\code{NULL}). Note when \code{x0 > x1}, the axis
##' direction will be reversed (i.e. from larger values to small
##' values)
##' @param ylim y-axis limits; similar to \code{xlim}
##' @param xlab x-axis title
##' @param ylab y-axis title
##' @return A bar plot
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @family plots
##' @example inst/examples/qbar-ex.R
qbar =
    function(x, data = last_data(), space = 0.1, main = '', horizontal = FALSE,
             standardize = FALSE, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL) {
    data = check_data(data)
    b = brush(data)
    s = attr(data, 'Scales')
    meta =
        Bar.meta$new(var = as.character(as.list(match.call()[-1])$x), space = space,
                     alpha = 1, horizontal = horizontal, main = main,
                     standardize = standardize)
    compute_coords = function() {
        meta$value = factor(data[, meta$var], exclude = NULL)
        meta$nlevel = length(levels(meta$value))
        .find_split_var(data, meta)
        idx = visible(data)
        tmp = table(meta$value[idx], meta$value2[idx])
        if (ncol(tmp) > 1) tmp = t(apply(tmp, 1, cumsum))
        if (meta$standardize) tmp = tmp / tmp[, meta$nlevel2, drop = ncol(tmp) > 1]
        tmp[!is.finite(tmp)] = 0  # consider division by 0
        meta$y = c(tmp)
        meta$x = rep(meta$xat <- seq(meta$nlevel) + .5, meta$nlevel2)
        meta$yat = axis_loc(c(0, meta$y))
        meta$xlabels = rownames(tmp)
        meta$ylabels = format(meta$yat)
        meta$xlab = if (is.null(xlab)) meta$var else xlab
        meta$ylab = if (is.null(ylab)) '' else ylab
        w = diff(meta$xat[1:2]) / (1 + meta$space) / 2  # half width of a bar
        meta$xleft = meta$x - w; meta$xright = meta$x + w
        meta$ybottom = c(cbind(0, tmp[, -meta$nlevel2])); meta$ytop = meta$y
        meta$limits =
            extend_ranges(cbind(if (is.null(xlim))
                                range(c(meta$xleft, meta$xright)) else xlim,
                                if (is.null(ylim))
                                range(c(meta$ybottom, meta$ytop)) else ylim))
    }
    compute_coords()
    compute_colors = function() {
        .bar_compute_colors(data, meta)
    }
    compute_colors()
    flip_coords = function() {
        .bar_flip_coords(data, meta)
    }
    flip_coords()
    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15
    main_draw = function(layer, painter) {
        .bar_draw_main(layer, painter, meta)
    }
    brush_draw = function(layer, painter) {
        .bar_draw_brush(layer, painter, data, meta)
    }
    brush_mouse_press = function(layer, event) {
        common_mouse_press(layer, event, data, meta)
    }
    brush_mouse_move = function(layer, event) {
        rect = qrect(update_brush_size(meta, event))
        hits = layer$locate(rect)
        if (length(hits)) {
            hits = .find_intersect(meta$value, meta$value2, hits, meta$nlevel)
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
    identify_hover = function(layer, event) {
        if (!b$identify) return()
        b$cursor = 2L
        meta$pos = as.numeric(event$pos())
        meta$identified = layer$locate(identify_rect(meta))
        qupdate(layer.identify)
    }
    identify_draw = function(layer, painter) {
        if (!b$identify || !length(idx <- meta$identified)) return()
        k = .find_intersect(meta$value, meta$value2, idx, meta$nlevel)
        vis = visible(data) & k
        meta$identify.labels =
            sprintf('%s = %s%s\ncounts: %s\nproportion: %.2f%%',
                    meta$var, shQuote(paste(unique(meta$value[k]), collapse = ', ')),
                    if (length(meta$var2)) {
                        sprintf('\n(%s = %s)', meta$var2,
                                paste(shQuote(unique(meta$value2[k])), collapse = ', '))
                    } else '',
                    sum(vis), mean(vis) * 100)
        draw_identify(layer, painter, data, meta)
        idx = idx + 1
        qdrawRect(painter, meta$xleft[idx], meta$ybottom[idx], meta$xright[idx],
                  meta$ytop[idx], stroke = b$color, fill = NA)
    }
    scene = qscene()
    layer.root = qlayer(scene)
    layer.main =
        qlayer(paintFun = main_draw,
               mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_release,
               mouseMove = brush_mouse_move, hoverMoveFun = identify_hover,
               keyPressFun = key_press, keyReleaseFun = key_release,
               limits = qrect(meta$limits), clip = TRUE)
    layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
    layer.identify = qlayer(paintFun = identify_draw, limits = qrect(meta$limits))
    layer.title = qmtext(meta = meta, side = 3)
    layer.xlab = qmtext(meta = meta, side = 1)
    layer.ylab = qmtext(meta = meta, side = 2)
    layer.xaxis = qaxis(meta = meta, side = 1)
    layer.yaxis = qaxis(meta = meta, side = 2)
    layer.grid = qgrid(meta = meta, minor = ifelse(meta$horizontal, 'x', 'y'))
    layer.root[0, 2] = layer.title
    layer.root[2, 2] = layer.xaxis
    layer.root[3, 2] = layer.xlab
    layer.root[1, 1] = layer.yaxis
    layer.root[1, 0] = layer.ylab
    layer.root[1, 2] = layer.grid
    layer.root[1, 2] = layer.main
    layer.root[1, 2] = layer.brush
    layer.root[1, 2] = layer.identify
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
    view$setWindowTitle(sprintf('Bar plot: %s', meta$var))
    meta$varChanged$connect(function() {
        view$setWindowTitle(sprintf('Bar plot: %s', meta$var))
    })
    d.idx = add_listener(data, function(i, j) {
        switch(j, .brushed = qupdate(layer.brush),
               .color = {
                   compute_colors()
                   qupdate(layer.main)
               }, {
                   compute_coords(); compute_colors(); flip_coords()
                   layer.main$invalidateIndex()
                   qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
                   qupdate(layer.main)
               })
    })
    qconnect(layer.main, 'destroyed', function(x) {
        ## b$colorChanged$disconnect(b.idx)
        remove_listener(data, d.idx)
    })

    b$cursorChanged$connect(function() {
        set_cursor(view, b$cursor)
    })
    sync_limits(meta, layer.main, layer.brush, layer.identify)
    meta$manual.brush = function(pos) {
        brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
    }
    attr(view, 'meta') = meta
    view
}

Bar.meta =
    setRefClass("Bar_meta",
                fields = signalingFields(c(

                Common.meta,

                list(var = 'character', value = 'factor',
                var2 = 'character', value2 = 'factor',
                nlevel = 'integer', nlevel2 = 'integer',
                x = 'numeric', y = 'numeric', space = 'numeric',
                xleft = 'numeric', xright = 'numeric',
                ybottom = 'numeric', ytop = 'numeric',
                horizontal = 'logical', freq = 'logical', standardize = 'logical',
                split.type = 'character')

                )))

.find_split_var = function(data, meta) {
    s = attr(data, 'Scales')
    for (i in c('color', 'border')) {
        if (length(nm <- s[[i]]$variable) && (nm %in% names(data)) &&
            is.factor(v <- data[, nm])) {
            meta$var2 = nm
            meta$value2 = factor(v, exclude = NULL)
            meta$split.type = i
            meta$nlevel2 = length(levels(meta$value2))
            return()
        }
    }
    meta$var2 = NULL
    meta$value2 = factor(character(nrow(data)))
    meta$split.type = NULL
    meta$nlevel2 = 1
}

.no_split_color = function(x) {
    if (length(unique(x)) == 1) x[1] else 'gray15'
}
.bar_compute_colors = function(data, meta) {
    if (!length(meta$var2)) {
        ## split variable was not set
        meta$color = .no_split_color(data$.color)
        meta$border = .no_split_color(data$.border)
    } else {
        ## higher priority on color than border
        meta$color = rep(tapply(data$.color, meta$value2, `[`, 1), each = meta$nlevel)
        meta$border = if (meta$split.type == 'color')
            meta$color else rep(tapply(data$.border, meta$value2, `[`, 1),
                                each = meta$nlevel)
    }
}
.bar_flip_coords = function(data, meta) {
    if (!meta$horizontal) return()
    switch_value('xat', 'yat', meta)
    switch_value('xlabels', 'ylabels', meta)
    switch_value('xlab', 'ylab', meta)
    switch_value('xleft', 'ybottom', meta)
    switch_value('xright', 'ytop', meta)
    meta$limits = meta$limits[, 2:1]
}
.bar_draw_main = function(layer, painter, meta) {
    ## deal with 0 pixel rect
    if (meta$horizontal) {
        if (any(idx <- meta$xright == 0))
            meta$xright[idx] = one_pixel(painter)[1]
    } else {
        if (any(idx <- meta$ytop == 0))
            meta$ytop[idx] = one_pixel(painter)[2]
    }
    qdrawRect(painter, meta$xleft, meta$ybottom, meta$xright, meta$ytop,
              stroke = meta$border, fill = meta$color)
}
.bar_draw_brush = function(layer, painter, data, meta) {
    b = brush(data)
    if (b$identify) return()
    if (any(idx <- selected(data) & (vis <- visible(data)))) {
        d = c(table(meta$value[idx], meta$value2[idx])) # brushed counts
        if (length(meta$freq) && !meta$freq)
            d = d / (sum(vis) * diff(meta$breaks[1:2])) * meta$multiplier
        if (isTRUE(meta$standardize)) {
            d = d / c(table(meta$value[vis]))
            d[!is.finite(d)] = 0
        }
        if (meta$horizontal)
            qdrawRect(painter, meta$xleft, meta$ybottom, meta$xleft + d, meta$ytop,
                      stroke = NA, fill = b$color) else
        qdrawRect(painter, meta$xleft, meta$ybottom, meta$xright, meta$ybottom + d,
                  stroke = NA, fill = b$color)
    }
    draw_brush(layer, painter, data, meta)
}
.find_intersect = function(x1, x2, idx, n) {
    h = logical(length(x1))
    for (i in idx)
        h =
            h | ((x1 %in% levels(x1)[(i %% n) + 1]) &
                 (x2 %in% levels(x2)[ceiling((i + 1) / n)]))
    h
}
