##' Create a bar plot
##'
##' Key events are documented in \code{\link{common_key_press}} and
##' \code{\link{common_key_release}}. Mouse events mainly include
##' brushing; as usual, left click to move the brush, and right click
##' to resize the brush.
##' @param x a variable name
##' @param data a mutaframe created by \code{\link{qdata}}
##' @param space the space between bars proportional to the width of bars
##' @param main the main title
##' @param horizontal \code{TRUE} to draw a horizontal plot or
##' \code{FALSE} (vertical)
##' @return A bar plot
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example inst/examples/qbar-ex.R
qbar = function(x, data = last_data(), space = 0.1, main = '', horizontal = FALSE) {
    data = check_data(data)
    b = brush(data)
    meta =
        Bar.meta$new(var = as.character(as.list(match.call()[-1])$x), space = space,
                     alpha = 1, horizontal = horizontal, main = main)
    compute_coords = function() {
        data[, meta$var] = factor(data[, meta$var], exclude = NULL)
        tmp = data[visible(data), meta$var]
        meta$y = c(table(tmp))
        meta$xat = meta$x = seq_along(meta$y)
        meta$yat = axis_loc(c(0, meta$y))
        meta$xlabels = names(meta$y)
        meta$ylabels = as.character(meta$yat)
        meta$xlab = meta$var
        meta$ylab = ''
    }
    compute_colors = function() {
        tmp = data[, meta$var]
        meta$stroke = tapply(data$.color, tmp, `[`, 1)
        meta$fill = tapply(data$.fill, tmp, `[`, 1)
    }
    compute_coords()
    compute_colors()
    flip_coords = function() {
        if (!meta$horizontal) return()
        switch_value('x', 'y', meta)
        switch_value('xat', 'yat', meta)
        switch_value('xlabels', 'ylabels', meta)
        switch_value('xlab', 'ylab', meta)
        switch_value('xleft', 'ybottom', meta)
        switch_value('xright', 'ytop', meta)
        meta$limits = meta$limits[, 2:1]
    }
    ## bars (rectangles)
    compute_bars = function() {
        w = diff(meta$xat[1:2]) / (1 + meta$space) / 2  # half width of a bar
        meta$xleft = meta$xat - w; meta$xright = meta$xat + w
        meta$ybottom = 0; meta$ytop = meta$y
        meta$limits =
            extend_ranges(cbind(range(c(meta$xleft, meta$xright)),
                                range(c(meta$ybottom, meta$ytop))))
    }
    compute_bars()
    flip_coords()
    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15
    main_draw = function(layer, painter) {
        qdrawRect(painter, meta$xleft, meta$ybottom, meta$xright, meta$ytop,
                  stroke = meta$stroke, fill = meta$fill)
    }
    brush_draw = function(layer, painter) {
        if (b$identify) return()
        if (any(idx <- selected(data) & visible(data))) {
            tmp = data[idx, meta$var]
            if (meta$horizontal)
                qdrawRect(painter, meta$xleft, meta$ybottom, c(table(tmp)), meta$ytop,
                          stroke = NA, fill = b$color) else
            qdrawRect(painter, meta$xleft, meta$ybottom, meta$xright, c(table(tmp)),
                      stroke = NA, fill = b$color)
        }
        draw_brush(layer, painter, data, meta)
    }
    brush_mouse_press = function(layer, event) {
        common_mouse_press(layer, event, data, meta)
    }
    brush_mouse_move = function(layer, event) {
        rect = qrect(update_brush_size(meta, event))
        hits = layer$locate(rect) + 1
        if (length(hits))
            hits = data[, meta$var] %in% levels(data[, meta$var])[hits]
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
    layer.root = qlayer(scene)
    layer.main =
        qlayer(paintFun = main_draw,
               mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_release,
               mouseMove = brush_mouse_move,
               keyPressFun = key_press, keyReleaseFun = key_release,
               focusInFun = function(layer, painter) {
                   focused(data) = TRUE
               }, focusOutFun = function(layer, painter) {
                   focused(data) = FALSE
               }, limits = qrect(meta$limits))
    layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
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
    meta$mainChanged$connect(function() {
        view$setWindowTitle(sprintf('Bar plot: %s', meta$var))
    })
    d.idx = add_listener(data, function(i, j) {
        switch(j, .brushed = qupdate(layer.brush),
               .color = {
                   compute_colors()
                   qupdate(layer.main)
               }, {
                   compute_coords(); compute_colors(); compute_bars(); flip_coords()
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
    sync_limits(meta, layer.main, layer.brush)  # sync limits of main & brush layers
    meta$manual.brush = function(pos) {
        brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
    }
    attr(view, 'meta') = meta
    view
}

Bar.meta =
    setRefClass("Bar_meta", fields =
                signalingFields(list(var = 'character', alpha = 'numeric',
                                     x = 'numeric', y = 'numeric',
                                     xat = 'numeric', yat = 'numeric',
                                     xlab = 'character', ylab = 'character',
                                     xlabels = 'character', ylabels = 'character',
                                     space = 'numeric', limits = 'matrix',
                                     xleft = 'numeric', xright = 'numeric',
                                     ybottom = 'numeric', ytop = 'numeric',
                                     stroke = 'character', fill = 'character',
                                     start = 'numeric', pos = 'numeric',
                                     brush.move = 'logical', brush.size = 'numeric',
                                     manual.brush = 'function', horizontal = 'logical',
                                     main = 'character')))
