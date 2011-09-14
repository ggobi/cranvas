##' Draw a univariate density plot
##'
##' Draw a univariate density plot, with a rug plot underneath.
##'
##' Common interactions are documented in
##' \code{\link{common_key_press}}. Specific interactions include:
##' Arrow \code{Up}/\code{Down} in-/de-creases size of points; Arrow
##' \code{Left}/\code{Right} de-/in-creases binwidth for density; Key
##' \code{Z} toggle zoom on/off (default is off); mouse click & drag
##' will specify a zoom window, reset to default window by click/no
##' drag; Key \code{X} toggles focal zoom on/off (default is off);
##' mouse click & drag will specify a zoom window, zoom out by
##' pressing \code{Shift} key; Key \code{R} resets data range to
##' original scale.
##' @param x variable name which designates variable displayed on the
##' horizontal axis
##' @inheritParams qbar
##' @export
##' @family plots
##' @example inst/examples/qdensity-ex.R
qdensity <- function(x, data = last_data(), binwidth = NULL, main = '',
                     xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, size=4) {

    ################################
    # data processing & parameters #
    ################################

    data = check_data(data)
    b = brush(data)
    s = attr(data, 'Scales')
    z = as.list(match.call()[-1])
    ## initialize meta
    meta =
        Dens.meta$new(xvar = as.character(z$x),
                      alpha = .5, main = main, minor = 'xy',
                      samesize = diff(range(data$.size, na.rm=TRUE, finite=TRUE)) < 1e-7, size = size)
    ## set default xlab if not provided
    if (is.null(xlab)) meta$xlab = meta$xvar

    ## reorder the points according to color/border for drawing speed
    compute_order = function() {
        ord = order(data$.color, data$.border) # the ideal order to draw
        names(ord) = seq(nrow(data))    # orignal order is in names
        meta$order = ord
    }
    compute_order()

    ## compute coordinates/axes-related stuff
    compute_coords = function() {
        meta$x = data[, meta$xvar]
        if (is.null(binwidth))
            meta$binwidth <- density(meta$x)$bw # Get density to estimate the best binwidth
        idx = visible(data)
        grp = data$.color
        if (length(nm <- s$color$variable) && (nm %in% names(data))) {
            grp = if (is.factor(data[, nm])) {
                data$.color
            } else rep('gray15', length(meta$x))
        }
        ## densities by color groups
        meta$dxy =
            sapply(split(meta$x[idx], grp[idx]), function(v) {
                density(v, meta$binwidth)[c('x', 'y')]
            }, simplify = FALSE)
        y.all = as.vector(sapply(meta$dxy, `[[`, 'y')) # all density values
        meta$xat = axis_loc(meta$x[idx]); meta$yat = axis_loc(y.all)
        meta$xlabels = format(meta$xat); meta$ylabels = format(meta$yat)
        meta$xlab = if (is.null(xlab)) meta$xvar else xlab
        meta$ylab = if (is.null(ylab)) "Density" else ylab
        y.all = y.all * 100 + 0.00  # due to Qt imprecision bug
        meta$yat = meta$yat * 100 + 0.00
        r =
            cbind(if (is.null(xlim))
                  range(meta$x[idx], na.rm = TRUE, finite = TRUE) else xlim,
                  if (is.null(ylim))
                  c(0.00, max(y.all, na.rm = TRUE)) else ylim * 100 + 0.00)
        message("r ", length(r), " ", r[1], " ", r[2], " ", r[3], " ", r[4])
        meta$limits = extend_ranges(r, f=c(0.15, 0.15))
        message("lims ", length(meta$limits), " ", meta$limits[1], " ", meta$limits[2], " ", meta$limits[3], " ", meta$limits[4])
        meta$x = meta$x[meta$order]
        meta$y = diff(meta$limits[, 2]) / 80  # ugly clipping bug
    }
    compute_coords()

    ## aesthetics (colors)
    compute_aes = function() {
        idx = !visible(data)[meta$order]
        meta$color = data$.color[meta$order]; meta$border = data$.border[meta$order]
        meta$color[idx] = NA; meta$border[idx] = NA
        meta$size = data$.size[meta$order]; meta$size[idx] = NA
    }
    compute_aes()

    ## initialize brush size (1/15 of the layer size)
    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15

    ## draw points & density
    main_draw = function(layer, painter) {
        if (meta$samesize) {
            qdrawGlyph(painter, qglyphCircle(r = data$.size[1]), meta$x, meta$y,
                       stroke = alpha(meta$border, meta$alpha), fill = alpha(meta$color, meta$alpha))
        } else {
            qdrawCircle(painter, meta$x, meta$y, r = meta$size,
                        stroke = alpha(meta$border, meta$alpha), fill = alpha(meta$color, meta$alpha))
        }
    }
    ## density lines
    line_draw = function(layer, painter) {
        for (i in names(meta$dxy)) {
            xy = meta$dxy[[i]]
            qlineWidth(painter) = 2
            qdrawLine(painter, x = xy$x, y = xy$y * 100, stroke = i)
        }
    }
    ## draw brushed points
    brush_draw = function(layer, painter) {
        if (b$identify) return()
        idx = visible(data) & selected(data)
        if (any(idx)) {
            if (meta$samesize) {
                qdrawGlyph(painter, qglyphCircle(r = b$size * meta$size[1]),
                           data[idx, meta$xvar], meta$y, stroke = b$color, fill = b$color)
            } else {
                qdrawCircle(painter, data[idx, meta$xvar], meta$y,
                            r = b$size * data$.size[idx], stroke = b$color, fill = b$color)
            }
            dxy = density(data[idx, meta$xvar], meta$binwidth)
            qlineWidth(painter) = 3
            qdrawLine(painter, dxy$x, dxy$y * 100, stroke = b$color)
        }
        draw_brush(layer, painter, data, meta)
    }

    ## events
    brush_mouse_press = function(layer, event) {
        common_mouse_press(layer, event, data, meta)
    }
    brush_mouse_move = function(layer, event) {
        rect = qrect(update_brush_size(meta, event))
        hits = layer$locate(rect) + 1
        if (length(hits)) {
            hits = intersect(meta$order[as.character(hits)],  which(visible(data)))
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
        shift = event$modifiers() == Qt$Qt$ShiftModifier
        if (shift && length(i <- which(match_key(c('Left', 'Right', 'Up', 'Down'))))) {
            j = c(1, 1, 2, 2)[i]; k = c(1, -1, -1, 1)[i]
            meta$limits[, j] = extend_ranges(meta$limits[, j], k * c(1, -1) * 0.02)
        } else if (length(i <- which(match_key(c('Up', 'Down'))))) {
            ## change size
            data$.size = pmax(0.1, c(1.1, 0.9)[i] * data$.size)
        }
    }
    key_release = function(layer, event) {
        common_key_release(layer, event, data, meta)
    }
    mouse_wheel = function(layer, event) {
        meta$limits[, 1] = extend_ranges(meta$limits[, 1], -sign(event$delta()) * 0.05)
    }
    identify_hover = function(layer, event) {
        if (!b$identify) return()
        b$cursor = 2L
        meta$pos = as.numeric(event$pos())
        hits = layer$locate(identify_rect(meta)) + 1
        meta$identified = intersect(meta$order[as.character(hits)], which(visible(data)))
        qupdate(layer.identify)
    }
    identify_draw = function(layer, painter) {
        if (!b$identify || !length(idx <- meta$identified)) return()
        meta$identify.labels =
            sprintf('row id: %s\n%s: %s',
                    paste(rownames(data)[idx], collapse = ', '),
                    meta$xvar, paste(meta$x[idx], collapse = ', '))
        draw_identify(layer, painter, data, meta)
        if (meta$samesize) {
            qdrawGlyph(painter, qglyphCircle(r = 2 * b$size * data$.size[1]),
                       data[idx, meta$xvar], meta$y, stroke = b$color, fill = NA)
        } else {
            qdrawCircle(painter, data[idx, meta$xvar], meta$y, r = b$size * meta$size,
                        stroke = b$color, fill = NA)
        }
    }

    ###################
    # draw the canvas #
    ###################
    scene <- qscene()
    layer.root <- qlayer(scene)

    layer.main =
        qlayer(paintFun = main_draw,
               mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_release,
               mouseMove = brush_mouse_move, hoverMoveFun = identify_hover,
               keyPressFun = key_press, keyReleaseFun = key_release,
               wheelFun = mouse_wheel,
               limits = qrect(meta$limits), clip = TRUE)
    layer.lines = qlayer(paintFun = line_draw, limits = qrect(meta$limits), clip = TRUE)
    layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits), clip = TRUE)
    layer.identify = qlayer(paintFun = identify_draw, limits = qrect(meta$limits))
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
    layer.root[1, 2] = layer.main
    layer.root[1, 2] = layer.lines
    layer.root[1, 2] = layer.brush
    layer.root[1, 2] = layer.identify
    layer.root[1, 3] = qlayer()

    ## set sizes of layers (arrange the layout)
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

    ## layout is dynamic (listen to changes in xlab/ylab/xlabels/ylabels...)
    meta$mainChanged$connect(set_layout)
    meta$xlabChanged$connect(set_layout); meta$ylabChanged$connect(set_layout)
    meta$xlabelsChanged$connect(set_layout); meta$ylabelsChanged$connect(set_layout)

    ## finally create the view and set window title
    view = qplotView(scene = scene)
    view$setWindowTitle(paste("Density plot:", meta$xvar))
    meta$xvarChanged$connect(function() {
        view$setWindowTitle(paste("Density plot:", meta$xvar))
    })

    ## listeners on the data (which column updates which layer(s))
    d.idx = add_listener(data, function(i, j) {
        idx = which(j == c(meta$xvar, '.brushed', '.color', '.border'))
        if (length(idx) < 1) {
            compute_coords(); compute_aes()
            meta$samesize = diff(range(data$.size, na.rm = TRUE, finite = TRUE)) < 1e-7
            qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
            layer.main$invalidateIndex(); qupdate(layer.main)
            return()
        } else idx = c(1, 2, 3, 3)[idx]
        switch(idx, compute_coords(), qupdate(layer.brush), {
            compute_order(); compute_aes(); qupdate(layer.main)
        })
    })

    ## when layer is destroyed, remove the listener from data
    qconnect(layer.main, 'destroyed', function(x) {
        ## b$colorChanged$disconnect(b.idx)
        remove_listener(data, d.idx)
    })

    ## when b$cursor is changed, update cursor on screen
    b$cursorChanged$connect(function() {
        set_cursor(view, b$cursor)
    })

    ## these layers have the same limits from meta$limits
    sync_limits(meta, layer.main, layer.lines, layer.brush, layer.identify)

    ## simulate brushing
    meta$manual.brush = function(pos) {
        brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
    }

    ## attach meta to the returned value (for post-processing or debugging)
    attr(view, 'meta') = meta
    view
}

Dens.meta =
  setRefClass("Dens_meta",
    fields = signalingFields(c(
      Common.meta,

      list(xvar = 'character', order = 'numeric',
        x = 'numeric', y = 'numeric', binwidth = 'numeric',
        dxy = 'list', asp = 'numeric', samesize = 'logical')
)))
