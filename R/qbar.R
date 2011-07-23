##' Create a bar plot.
##'
##'
##' @param x a variable name
##' @param data a mutaframe created by \code{\link{qdata}}
##' @param space the space between bars proportional to the width of bars
##' @param main the main title
##' @return A bar plot
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example cranvas/inst/examples/qbar-ex.R
qbar = function(x, data, space = 0.1, main) {
    b = brush(data)
    meta =
        Bar.meta$new(var = as.character(as.list(match.call()[-1])$x), space = space)
    if (missing(main)) main = paste("Bar plot of", deparse(substitute(data)))
    compute_coords = function() {
        tmp = data[, meta$var]
        tmp = as.factor(tmp)
        meta$y = c(table(tmp))
        meta$xat = meta$x = seq_along(meta$y)
        meta$yat = axis_loc(c(0, meta$y))
        meta$xlabels = names(meta$y)
        meta$ylabels = as.character(meta$yat)
        meta$xlab = meta$var
        meta$ylab = ''
        meta$stroke = tapply(data$.color, tmp, `[`, 1)
        meta$fill = tapply(data$.fill, tmp, `[`, 1)
    }
    compute_coords()
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
    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15
    main_draw = function(layer, painter) {
        qdrawRect(painter, meta$xleft, meta$ybottom, meta$xright, meta$ytop,
                  stroke = meta$stroke, fill = meta$fill)
    }
    brush_draw = function(layer, painter) {
        if (b$identify) return()
        if (any(is.na(meta$pos))) return()
        if (any(idx <- selected(data))) {
            tmp = as.factor(data[idx, meta$var])
            qdrawRect(painter, meta$xleft, meta$ybottom, meta$xright, c(table(tmp)),
                      stroke = NA, fill = b$color)
        }
        qlineWidth(painter) = b$style$linewidth
        qdrawRect(painter, meta$pos[1] - meta$brush.size[1],
                  meta$pos[2] - meta$brush.size[2], meta$pos[1], meta$pos[2],
                  stroke = b$style$color, fill = NA)
        qdrawCircle(painter, meta$pos[1], meta$pos[2], r = 1.5 * b$style$linewidth,
                    stroke = b$style$color, fill = b$style$color)
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
        if (b$identify) return()
        rect = qrect(update_brush_size(meta))
        hits = layer$locate(rect) + 1
        hits = data[, meta$var] %in% levels(as.factor(data[, meta$var]))[hits]
        selected(data) = mode_selection(selected(data), hits, mode = b$mode)
        self_link(data)
        ## on mouse release
        if (event$button() != Qt$Qt$NoButton) {
            b$cursor = 0L  # restore to Arrow cursor
            save_brush_history(data)  # store brushing history
        }
    }

    scene = qscene()
    layer.root = qlayer(scene)
    layer.main =
        qlayer(paintFun = main_draw,
               mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_move,
               mouseMove = brush_mouse_move,
               focusInFun = function(layer, painter) {
                   focused(data) = TRUE
               }, focusOutFun = function(layer, painter) {
                   focused(data) = FALSE
               }, limits = qrect(meta$limits))
    layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
    layer.title = qmtext(data = meta, side = 3, text = main, sister = layer.main)
    layer.xlab = qmtext(data = meta, side = 1, text = meta$xlab, sister = layer.main)
    layer.ylab = qmtext(data = meta, side = 2, text = meta$ylab, sister = layer.main)
    layer.xaxis = qaxis(data = meta, side = 1, sister = layer.main)
    layer.yaxis = qaxis(data = meta, side = 2, sister = layer.main)
    layer.grid = qgrid(data = meta, sister = layer.main, minor = 'y')
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
               .color = qupdate(layer.main), {
                   compute_coords()
                   compute_bars()
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

    attr(view, 'meta') = meta
    view
}

Bar.meta =
    setRefClass("Bar_meta", fields =
                signalingFields(list(var = 'character',
                                     x = 'numeric', y = 'numeric',
                                     xat = 'numeric', yat = 'numeric',
                                     xlab = 'character', ylab = 'character',
                                     xlabels = 'character', ylabels = 'character',
                                     space = 'numeric',
                                     xleft = 'numeric', xright = 'numeric',
                                     ybottom = 'numeric', ytop = 'numeric',
                                     stroke = 'character', fill = 'character',
                                     start = 'numeric', pos = 'numeric',
                                     brush.move = 'logical', brush.size = 'numeric')))
