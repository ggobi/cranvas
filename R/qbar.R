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
##' @param x a variable name (will be coerced to a factor if it is
##' not; \code{NA} will also be a level of the factor if the variable
##' has any \code{NA}'s)
##' @param data a mutaframe created by \code{\link{qdata}}
##' @param space the space between bars proportional to the width of
##' bars
##' @param main the main title
##' @param horizontal \code{TRUE} to draw a horizontal plot or
##' \code{FALSE} (vertical)
##' @param standardize logical: whether to standardize the height of
##' each bar to 1
##' @return A bar plot
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example inst/examples/qbar-ex.R
qbar =
    function(x, data = last_data(), space = 0.1, main = '', horizontal = FALSE,
             standardize = FALSE) {
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
        meta$xat = meta$x = rep(seq(meta$nlevel), meta$nlevel2)
        meta$yat = axis_loc(c(0, meta$y))
        meta$xlabels = rownames(tmp)
        meta$ylabels = format(meta$yat)
        meta$xlab = meta$var
        meta$ylab = ''
        w = diff(meta$xat[1:2]) / (1 + meta$space) / 2  # half width of a bar
        meta$xleft = meta$xat - w; meta$xright = meta$xat + w
        meta$ybottom = c(cbind(0, tmp[, -meta$nlevel2])); meta$ytop = meta$y
        meta$limits =
            extend_ranges(cbind(range(c(meta$xleft, meta$xright)),
                                range(c(meta$ybottom, meta$ytop))))
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
        ## indices outside the range should be discarded
        hits = discard(layer$locate(rect), c(0, meta$nlevel * meta$nlevel2 - 1))
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
               focusInFun = function(layer, painter) {
                   focused(data) = TRUE
               }, focusOutFun = function(layer, painter) {
                   focused(data) = FALSE
               }, limits = qrect(meta$limits))
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
    layout = layer.root$gridLayout()
    layout$setRowPreferredHeight(0, prefer_height(meta$main))
    layout$setRowPreferredHeight(2, prefer_height(meta$xlabels))
    layout$setRowPreferredHeight(3, prefer_height(meta$xlab))
    layout$setColumnPreferredWidth(0, prefer_width(meta$ylab, FALSE))
    layout$setColumnPreferredWidth(1, prefer_width(meta$ylabels))
    layout$setColumnMaximumWidth(3, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setRowStretchFactor(2, 0)
    layout$setRowStretchFactor(3, 0)
    layout$setColumnStretchFactor(0, 0)
    layout$setColumnStretchFactor(1, 0)
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
    setRefClass("Bar_meta", fields =
                signalingFields(list(var = 'character', value = 'factor', alpha = 'numeric',
                                     x = 'numeric', y = 'numeric',
                                     xat = 'numeric', yat = 'numeric',
                                     xlab = 'character', ylab = 'character',
                                     xlabels = 'character', ylabels = 'character',
                                     space = 'numeric', limits = 'matrix',
                                     xleft = 'numeric', xright = 'numeric',
                                     ybottom = 'numeric', ytop = 'numeric',
                                     color = 'character', border = 'character',
                                     start = 'numeric', pos = 'numeric',
                                     brush.move = 'logical', brush.size = 'numeric',
                                     manual.brush = 'function', horizontal = 'logical',
                                     main = 'character', freq = 'logical',
                                     var2 = 'character', value2 = 'factor',
                                     split.type = 'character', identified = 'integer',
                                     identify.labels = 'character',
                                     standardize = 'logical',
                                     nlevel = 'integer', nlevel2 = 'integer')))

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
    switch_value('x', 'y', meta)
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
        if (any(idx <- meta$xright == meta$xleft))
            meta$xright[idx] = meta$xright[idx] + one_pixel(painter)[1]
    } else {
        if (any(idx <- meta$ytop == meta$ybottom))
            meta$ytop[idx] = meta$ytop[idx] + one_pixel(painter)[2]
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
            d = d / (sum(vis) * diff(meta$breaks[1:2]))
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
