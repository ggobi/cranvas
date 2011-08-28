##' Draw a histogram
##'
##' Draw an interactive histogram based on a continuous variable,
##' optionally split by a categorical variable. It supports some
##' common keyboard interactions (see \code{\link{common_key_press}})
##' as well as other interactions specific to histograms.
##'
##' The splitting variable is usually specified in \code{\link{qdata}}
##' as the \code{color} or \code{border} argument; if it is present,
##' each bar in the histogram will be split into categories.
##'
##' Arrow keys can be used to change the binwidth as well as the
##' breakpoints in the histograme. Up and Down can increase and
##' decrease the binwidth respectively; Left and Right can move the
##' breakpoints of the bins to the left (smaller) or right (larger).
##'
##' In the identify mode, the breakpoints of the bin(s) as well as
##' counts and proportion of cases in the bin(s) are shown as text
##' labels in the plot.
##' @param x the name of the numeric variable to be used to draw the
##' histogram
##' @inheritParams qbar
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
##' @example inst/examples/qhist-ex.R
qhist =
    function(x, data = last_data(), breaks = 30, freq = TRUE, main = '', horizontal = FALSE,
             spine = FALSE) {
    data = check_data(data)
    b = brush(data)
    meta =
        Hist.meta$new(var = as.character(as.list(match.call()[-1])$x), freq = freq,
                      alpha = 1, horizontal = horizontal, main = main, breaks = breaks,
                      standardize = spine, spine = spine)
    compute_coords = function() {
        if (meta$spine) meta$freq = meta$standardize = TRUE else meta$standardize = FALSE
        idx = visible(data)
        hst = hist(data[idx, meta$var], breaks = meta$breaks, plot = FALSE)
        if (!identical(meta$breaks, hst$breaks)) meta$breaks = hst$breaks
        meta$value =
            cut(data[, meta$var], breaks = meta$breaks, include.lowest = TRUE)
        meta$nlevel = length(levels(meta$value))
        .find_split_var(data, meta)
        tmp = table(meta$value[idx], meta$value2[idx])
        if (ncol(tmp) > 1) tmp = t(apply(tmp, 1, cumsum))
        if (!meta$freq) tmp = tmp / (sum(idx) * diff(hst$breaks[1:2]))
        if (meta$standardize) tmp = tmp / tmp[, meta$nlevel2, drop = ncol(tmp) > 1]
        tmp[!is.finite(tmp)] = 0  # consider division by 0
        meta$y = c(tmp)
        meta$x = rep(hst$mids, meta$nlevel2)
        meta$xat = axis_loc(meta$breaks); meta$yat = axis_loc(c(0, meta$y))
        meta$xlabels = format(meta$xat)
        meta$ylabels = format(meta$yat)
        meta$xlab = meta$var
        meta$ylab = if (meta$spine) 'Proportion' else if (meta$freq) 'Frequency' else 'Density'
        nb = length(hst$breaks)
        if (meta$spine) {
            meta$xright = cumsum(table(meta$value[idx])) / sum(idx)  # [0,1], prop counts
            meta$xleft = c(0, meta$xright[-meta$nlevel])
            meta$xat <- c(0, meta$xright)
            meta$xlabels = unname(tapply(formatC(meta$breaks), meta$xat, function(x) {
                if (length(x) <= 1) x else sprintf('%s%s(%s)',
                          x[1], if (meta$horizontal) '' else '\n', x[length(x)])
            }))
            meta$xat = unique(meta$xat)
            meta$xleft = rep(meta$xleft, meta$nlevel2)
            meta$xright = rep(meta$xright, meta$nlevel2)
        } else {
            meta$xleft = rep(hst$breaks[-nb], meta$nlevel2)
            meta$xright = rep(hst$breaks[-1], meta$nlevel2)
        }
        meta$ybottom = c(cbind(0, tmp[, -meta$nlevel2, drop = FALSE])); meta$ytop = meta$y
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
        meta$identify.labels =
            sprintf('bin: (%s]\ncount: %s\nproportion: %.2f%%',
                    paste(meta$breaks[range(idx %% meta$nlevel) + c(1, 2)], collapse = ','),
                    sum(k), mean(k) * 100)
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
    layer.grid = qgrid(meta = meta, minor = ifelse(meta$spine,
                                    ifelse(meta$horizontal, 'x', 'y'), 'xy'))
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
    view$setWindowTitle(paste(ifelse(meta$spine, "Spine plot:", "Histogram:"), meta$var))
    meta$varChanged$connect(function() {
        view$setWindowTitle(paste(ifelse(meta$spine, "Spine plot:", "Histogram:"), meta$var))
    })
    d.idx = add_listener(data, function(i, j) {
        switch(j, .brushed = qupdate(layer.brush),
               .color = {
                   compute_colors()
                   qupdate(layer.main)
               }, {
                   compute_coords(); compute_colors(); flip_coords()
                   qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
                   layer.main$invalidateIndex()
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
    sync_limits(meta, layer.main, layer.brush, layer.identify)  # sync limits
    meta$manual.brush = function(pos) {
        brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
    }
    meta$breaksChanged$connect(function () {
        compute_coords(); compute_colors(); flip_coords()
        layer.main$invalidateIndex()
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
                                     border = 'character', color = 'character',
                                     start = 'numeric', pos = 'numeric',
                                     brush.move = 'logical', brush.size = 'numeric',
                                     manual.brush = 'function', horizontal = 'logical',
                                     main = 'character', value = 'factor',
                                     var2 = 'character', value2 = 'factor',
                                     split.type = 'character', spine = 'logical',
                                     nlevel = 'integer', nlevel2 = 'integer',
                                     freq = 'logical', standardize = 'logical',
                                     identified = 'numeric', identify.labels = 'character')))
