##' Draw a histogram
##'
##' Draw an interactive histogram. It supports some common keyboard
##' interactions (see \code{\link{common_key_press}}) as well as other
##' interactions specific to histograms.
##'
##' Arrow keys can be used to change the binwidth as well as the
##' breakpoints in the histograme. Up and Down can increase and
##' decrease the binwidth respectively; Left and Right can move the
##' breakpoints of the bins to the left (smaller) or right (larger).
##' @param x the name of the numeric variable to be used to draw the
##' histogram
##' @param data a mutaframe created by \code{\link{qdata}}
##' @param breaks a single number giving the number of bins, or a
##' numeric vector giving the breakpoints
##' @param freq draw the frequencies (\code{TRUE}) or densities
##' (\code{FALSE})
##' @param main the main title (default to be \code{"Histogram of
##' variable name"})
##' @param horizontal whether to draw a horizontal or vertical plot
##' @return A histogram
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example inst/examples/qhist2-ex.R
qhist2 = function(x, data, breaks = 30, freq = TRUE, main, horizontal = FALSE) {
    b = brush(data)
    if (missing(main)) main = paste("Histogram of", deparse(substitute(x)))
    meta =
        Hist.meta$new(var = as.character(as.list(match.call()[-1])$x),
                     alpha = 1, horizontal = horizontal, main = main, breaks = breaks)
    compute_coords = function() {
        tmp = hist(data[visible(data), meta$var], breaks = meta$breaks, plot = FALSE)
        if (!identical(meta$breaks, tmp$breaks)) meta$breaks = tmp$breaks
        meta$y = if (freq) tmp$counts else tmp$density
        meta$x = tmp$mids
        meta$xat = axis_loc(meta$breaks); meta$yat = axis_loc(c(0, meta$y))
        meta$xlabels = format(meta$xat)
        meta$ylabels = format(meta$yat)
        meta$xlab = meta$var
        meta$ylab = if (freq) 'Frequency' else 'Density'
        meta$xleft = tmp$breaks[-length(tmp$breaks)]; meta$xright = tmp$breaks[-1]
        meta$ybottom = 0; meta$ytop = meta$y
        meta$limits =
            extend_ranges(cbind(range(c(meta$xleft, meta$xright)),
                                range(c(meta$ybottom, meta$ytop))))
    }
    ## average the colors in the bins; may not be a good idea but simpler to implement
    average_colors = function(x, f) {
        f = factor(f, levels = seq(max(f, na.rm = TRUE)))
        z = sapply(split(as.data.frame(t(col2rgb(x, alpha = TRUE))), f), colMeans) / 255
        z[is.na(z)] = 0
        rgb(z[1, ], z[2, ], z[3, ], z[4, ])
    }
    ## rows belong to which interval
    compute_intervals = function() {
        meta$intervals =
            cut(data[, meta$var], breaks = meta$breaks, labels = FALSE,
                include.lowest = TRUE)
    }
    compute_colors = function() {
        meta$stroke = average_colors(data$.color, meta$intervals)
        meta$fill = average_colors(data$.fill, meta$intervals)
    }
    compute_coords()
    compute_intervals()
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
    flip_coords()
    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15
    main_draw = function(layer, painter) {
        qdrawRect(painter, meta$xleft, meta$ybottom, meta$xright, meta$ytop,
                  stroke = meta$stroke, fill = meta$fill)
    }
    brush_draw = function(layer, painter) {
        if (b$identify) return()
        if (any(idx <- selected(data) & visible(data))) {
            tmp = hist(data[idx, meta$var], breaks = meta$breaks, plot = FALSE)
            ynew = if (freq) tmp$counts else tmp$density * mean(selected(data))
            if (meta$horizontal)
                qdrawRect(painter, meta$xleft, meta$ybottom, ynew, meta$ytop,
                          stroke = NA, fill = b$color) else
            qdrawRect(painter, meta$xleft, meta$ybottom, meta$xright, ynew,
                      stroke = NA, fill = b$color)
        }
        draw_brush(painter, b, meta)
    }
    brush_mouse_press = function(layer, event) {
        meta$start = as.numeric(event$pos())
        ## on right click, we can resize the brush; left click: only move the brush
        if (event$button() == Qt$Qt$RightButton) {
            b$cursor = 2L
            meta$brush.move = FALSE
        } else if (event$button() == Qt$Qt$LeftButton) {
            b$cursor = 0L
            meta$brush.move = TRUE
        }
    }
    brush_mouse_move = function(layer, event) {
        rect = qrect(update_brush_size(meta, event))
        hits = layer$locate(rect) + 1L
        if (length(hits))
            hits = meta$intervals %in% hits
        selected(data) = mode_selection(selected(data), hits, mode = b$mode)
        self_link(data)
    }
    brush_mouse_release = function(layer, event) {
        brush_mouse_move(layer, event)
        b$cursor = 0L  # restore to Arrow cursor
        save_brush_history(data)  # store brushing history
    }
    key_press = function(layer, event) {
        common_key_press(layer, event, data, meta)
        brk = meta$breaks
        if (length(i <- which(match_key(c('Up', 'Down'))))) {
            if ((nb <- length(brk) + c(-1, 1)[i]) > 1)
                meta$breaks = seq(min(brk), max(brk), length.out = nb)  # more/less bins
            return()
        } else if (length(i <- which(match_key(c('Left', 'Right'))))) {
            r = range(data[visible(data), meta$var], na.rm = TRUE)
            brk = brk + c(-1, 1)[i] * (brk[2] - brk[1]) / 50  # shift by +/-(2% bin)
            if (min(brk) > r[1]) brk = c(2 * brk[1] - brk[2], brk)
            if (max(brk) < r[2]) brk = c(brk, tail(brk, 1) + brk[2] - brk[1])
            if (length(brk) <= 2) return()
            ## see if two breakpoints both < min or > max (remove one if so)
            if (all(head(brk, 2) <= r[1])) brk = brk[-1]
            if (all(tail(brk, 2) >= r[2])) brk = brk[-length(brk)]
            meta$breaks = brk
            return()
        }
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
    layer.grid = qgrid(meta = meta, minor = 'xy')
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
    layout$setRowPreferredHeight(0, 30)
    layout$setRowPreferredHeight(2, 15 * max(sapply(gregexpr('\\n', meta$xlabels),
                              function(xx) ifelse(any(xx <0), 0, length(xx)) + 2)))
    layout$setRowPreferredHeight(3, 20)
    layout$setColumnPreferredWidth(0, 10)
    layout$setColumnPreferredWidth(1, 9 * max(nchar(unlist(strsplit(meta$ylabels, '\n')))) + 5)
    layout$setColumnMaximumWidth(3, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setRowStretchFactor(2, 0)
    layout$setRowStretchFactor(3, 0)
    layout$setColumnStretchFactor(0, 0)
    layout$setColumnStretchFactor(1, 0)
    view = qplotView(scene = scene)
    view$setWindowTitle(main)

    d.idx = add_listener(data, function(i, j) {
        switch(j, .brushed = qupdate(layer.brush),
               .color = {
                   compute_colors()
                   qupdate(layer.main)
               }, {
                   compute_coords(); compute_colors(); flip_coords()
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
    meta$breaksChanged$connect(function () {
        compute_coords(); compute_intervals(); compute_colors(); flip_coords()
        qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis); qupdate(layer.main)
    })
    attr(view, 'meta') = meta
    view
}

Hist.meta =
    setRefClass("Hist_meta", fields =
                signalingFields(list(var = 'character', alpha = 'numeric',
                                     x = 'numeric', y = 'numeric',
                                     xat = 'numeric', yat = 'numeric',
                                     xlab = 'character', ylab = 'character',
                                     xlabels = 'character', ylabels = 'character',
                                     breaks = 'numeric', limits = 'matrix',
                                     xleft = 'numeric', xright = 'numeric',
                                     ybottom = 'numeric', ytop = 'numeric',
                                     stroke = 'character', fill = 'character',
                                     start = 'numeric', pos = 'numeric',
                                     brush.move = 'logical', brush.size = 'numeric',
                                     manual.brush = 'function', horizontal = 'logical',
                                     main = 'character', intervals = 'integer')))
