##' Draw a histogram or a spine plot
##'
##' Draw an interactive histogram or spine plot based on a continuous
##' variable, optionally split by a categorical variable. It supports
##' some common keyboard interactions (see
##' \code{\link{common_key_press}}) as well as other interactions
##' specific to histograms and spine plots.
##'
##' The splitting variable is usually specified in \code{\link{qdata}}
##' as the \code{color} or \code{border} argument; if it is present,
##' each bar in the plot will be split into categories.
##'
##' Arrow keys can be used to change the binwidth as well as the
##' breakpoints in the plot. Up and Down can increase and decrease the
##' binwidth respectively; Left and Right can move the breakpoints of
##' the bins to the left (smaller) or right (larger).
##'
##' In the identify mode, the breakpoints of the bin(s) as well as
##' counts and proportion of cases in the bin(s) are shown as text
##' labels in the plot.
##'
##' The function \code{\link{qspine}} is a short-hand version of
##' \code{qhist(..., spine = TRUE)}.
##' @param x the name of the numeric variable to be used to draw the
##' histogram or spine plot
##' @param bins the desired number of bins
##' @param binwidth the bin width (\code{range(x) / bins} by default)
##' @param freq draw the frequencies (\code{TRUE}) or densities
##' (\code{FALSE}) (only applies to histogram)
##' @param spine if \code{TRUE}, draw a spine plot (bar widths
##' proportional to counts instead of being equal)
##' @inheritParams qbar
##' @return A histogram or a spine plot
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @family plots
##' @example inst/examples/qhist-ex.R
qhist =
    function(x, data = last_data(), bins = 30, binwidth = NULL, freq = TRUE,
             main = '', horizontal = FALSE, spine = FALSE, xlim = NULL, ylim = NULL) {
    data = check_data(data)
    b = brush(data)
    meta =
        Hist.meta$new(var = as.character(as.list(match.call()[-1])$x), freq = freq,
                      alpha = 1, horizontal = horizontal, main = main,
                      standardize = spine, spine = spine, multiplier = 1)
    initial_bins = function(default = TRUE) {
        d = data[, meta$var]
        ## temporarily steal from hadley's densityvis
        r = range(d, na.rm = TRUE, finite = TRUE)
        if (diff(r) < 1e-7 && default) {
            meta$breaks = r
            meta$binwidth = diff(r)
        } else {
            if (default) meta$binwidth = if (is.null(binwidth)) diff(r) / bins else binwidth
            meta$breaks = seq(r[1], r[2] + meta$binwidth, meta$binwidth) - meta$binwidth / 2
        }
    }
    initial_bins()
    compute_coords = function() {
        if (meta$spine) meta$freq = meta$standardize = TRUE else meta$standardize = FALSE
        idx = visible(data)
        meta$value = cut(data[, meta$var], breaks = meta$breaks, include.lowest = TRUE)
        nb = length(meta$breaks)
        meta$nlevel = nb - 1
        .find_split_var(data, meta)
        tmp = table(meta$value[idx], meta$value2[idx])
        if (ncol(tmp) > 1) tmp = t(apply(tmp, 1, cumsum))
        if (!meta$freq) tmp = tmp / (sum(idx) * meta$binwidth)
        if (meta$standardize) tmp = tmp / tmp[, meta$nlevel2, drop = ncol(tmp) > 1]
        tmp[!is.finite(tmp)] = 0  # consider division by 0
        meta$y = c(tmp)
        meta$x = rep(meta$breaks[-1] - meta$binwidth / 2, meta$nlevel2)
        meta$xat = axis_loc(meta$breaks); meta$yat = axis_loc(c(0, meta$y))
        meta$xlabels = format(meta$xat)
        meta$ylabels = format(meta$yat)
        meta$xlab = meta$var
        meta$ylab = if (meta$spine) 'Proportion' else if (meta$freq) 'Frequency' else 'Density'
        if (meta$spine) {
            meta$xright = cumsum(table(meta$value[idx])) / sum(idx)  # [0,1], prop counts
            meta$xleft = c(0, meta$xright[-meta$nlevel])
            meta$xat = c(0, meta$xright)
            meta$xlabels = unname(tapply(formatC(meta$breaks), meta$xat, function(x) {
                if (length(x) <= 1) x else sprintf('%s%s(%s)',
                          x[1], if (meta$horizontal) '' else '\n', x[length(x)])
            }))
            meta$xat = unique(meta$xat)
            meta$xleft = rep(meta$xleft, meta$nlevel2)
            meta$xright = rep(meta$xright, meta$nlevel2)
        } else {
            meta$xleft = rep(meta$breaks[-nb], meta$nlevel2)
            meta$xright = rep(meta$breaks[-1], meta$nlevel2)
        }
        meta$ybottom = c(cbind(0, tmp[, -meta$nlevel2, drop = FALSE])); meta$ytop = meta$y
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
        if (length(i <- which(match_key(c('Up', 'Down'))))) {
            if (meta$binwidth >= 1e-7) {
                meta$binwidth = c(1.05, 0.95)[i] * meta$binwidth  # larger/smaller bins
                initial_bins(default = FALSE)  # use new binwidth
            } else message('binwidth too small!')
            return()
        } else if (length(i <- which(match_key(c('Left', 'Right'))))) {
            brk = meta$breaks
            r = range(data[, meta$var], na.rm = TRUE, finite = TRUE)
            brk = brk + c(-1, 1)[i] * meta$binwidth / 50  # shift by +/-(2% bin)
            if (min(brk) > r[1]) brk = c(brk[1] - meta$binwidth, brk)
            if (max(brk) < r[2]) brk = c(brk, tail(brk, 1) + meta$binwidth)
            if (length(brk) <= 2) return()
            ## see if two breakpoints both < min or > max (remove one if so)
            if (all(head(brk, 2) <= r[1])) {
                brk = brk[-1]
                message('removed one left-most bin because it does not contain data...')
            }
            if (all(tail(brk, 2) >= r[2])) {
                brk = brk[-length(brk)]
                message('removed one right-most bin because it does not contain data...')
            }
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
        idx = which(j == c(meta$var, '.brushed', '.color', '.border'))
        if (length(idx) < 1) {
            compute_coords(); compute_colors(); flip_coords()
            qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
            layer.main$invalidateIndex(); qupdate(layer.main)
            return()
        } else if (idx == 4) idx = 3
        switch(idx, initial_bins(), qupdate(layer.brush), {
            compute_colors(); qupdate(layer.main)
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
                                     binwidth = 'numeric', multiplier = 'numeric',
                                     identified = 'numeric', identify.labels = 'character')))
