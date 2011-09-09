#' Draw a univariate density plot
#'
#' Draw a univariate density plot, with a rug plot underneath.
#'
#' arrow up/down: in-/de-crease size of points
#'
#' arrow -/+: de-/in-crease alpha level (starts at alpha=1 by default)
#'
#' arrow left/right: de-/in-crease binwidth for density
#'
#' Key 'z' toggle zoom on/off (default is off): mouse click & drag will specify a zoom window, reset to default window by click/no drag
#'
#' Key 'x' toggle focal zoom on/off (default is off): mouse click & drag will specify a zoom window, zoom out by pressing shift key
#'
#' Key 'r' resets data range to original scale
#' @param x which designates variable displayed on the horizontal axis
#' @param data mutaframe data to use
#' @param size point size
#' @param alpha transparency level, 1=completely opaque
#' @param xlim = c(min, max) user specifed data range for the x axis, by default range(x)
#' @param ylim = c(min, max) user specifed data range for the y axis, by default range(y)
#' @param xlab label on horizontal axis, default is name of x variable
#' @param ylab label on vertical axis, default is name of y variable
#' @param cache boolean to turn cache on for layers, defaults to TRUE
#' @export
#' @family plots
#' @example inst/examples/qdensity-ex.R
qdensity <- function(x, data = last_data(), main = NULL, binwidth = NULL,
  size = 4, alpha = 0.5, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, asp = NULL, ...)
{

  ################################
  # data processing & parameters #
  ################################

  data = check_data(data)
  b = brush(data)
  z = as.list(match.call()[-1])
  ## initialize meta
  meta =
    Dens.meta$new(xvar = as.character(z$x),
      alpha = alpha, main = main, minor = 'xy',
      samesize = diff(range(data$.size, na.rm=TRUE, finite=TRUE)) < 1e-7)
  ## set default xlab if not provided
  if (is.null(xlab)) meta$xlab = meta$xvar

  ## reorder the points according to color/border for drawing speed
  compute_order = function() {
    ord = order(data$.color, data$.border)  # the ideal order to draw
    names(ord) = seq(nrow(data))  # orignal order is in names
    meta$order = ord
  }
  compute_order()

  ## compute coordinates/axes-related stuff
  compute_coords = function() {
    meta$x = data[meta$order, meta$xvar]
    meta$y = rep(0, length(meta$x))
    if (is.null(binwidth)) 
      meta$binwidth <- density(meta$x)$bw # Get density to estimate the best binwidth
    idx = visible(data)[meta$order]
    message("idx ", length(idx), " ", min(meta$x[idx]), " ", max(meta$x[idx]))
    densdata = density(meta$x[idx], meta$binwidth) # May need to run this by number of colors
    #meta$dx = densdata$x
    #meta$dy = densdata$y
    meta$xat = axis_loc(meta$x); meta$yat = axis_loc(densdata$y)
    message("density ", length(densdata$y), " ", densdata$y[1])
    meta$xlabels = format(meta$xat); meta$ylabels = format(meta$yat)
    meta$xlab = if (is.null(xlab)) meta$xvar else xlab
    meta$ylab = if (is.null(ylab)) "Count" else ylab
    r =
      cbind(if (is.null(xlim))
              range(meta$x[idx], na.rm = TRUE, finite = TRUE) else xlim,
            if (is.null(ylim))
              range(meta$y[idx], na.rm = TRUE, finite = TRUE) else ylim)
    if (!is.null(asp)) {
      meta$asp = asp / 1.5  # 1.5 = 600/400 from the default window size
      r = if (meta$asp * (rx <- abs(r[2, 1] - r[1, 1])) > (ry <- abs(r[2, 2] - r[1, 2]))) {
      ## expand ylim
            cbind(r[, 1], extend_ranges(r[, 2], (rx * meta$asp / ry - 1) / 2))
          } else {
      ## DOUBLE CHECK HERE
            cbind(extend_ranges(r[, 1], (ry / (rx * meta$asp) - 1) / 2), r[, 2])
          }
    }
    meta$limits = extend_ranges(r)
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
    message("here 1 ", length(meta$x), " ", length(meta$y), " ", range(meta$x), " ", meta$limits[1])
    message(meta$ylab, " ", length(meta$color), " ", length(meta$xlabels), " ", length(meta$ylabels))
    if (meta$samesize) {
      qdrawGlyph(painter, qglyphCircle(r = data$.size[1]), meta$x, meta$y,
        stroke = meta$border, fill = meta$color)
    } else {
      qdrawCircle(painter, meta$x, meta$y, r = meta$size,
        stroke = meta$border, fill = meta$color)
    }
    ncol <- unique(meta$color)
    for (i in 1:length(ncol)) {
      sc <- ncol[i]
      dx <- density(meta$x[meta$.color == sc], bw=meta$binwidth)
      qlineWidth(painter) <- 3
      qdrawLine(painter, x=dx$x, y=dx$y, stroke = alpha(sc, 1))
    }
    message("here 2")
  }
  
  ## draw brushed points
  brush_draw = function(layer, painter) {
    if (b$identify) return()
    idx = visible(data) & selected(data)
    if (any(idx)) {
      if (meta$samesize) {
        qdrawGlyph(painter, qglyphCircle(r = b$size * meta$size[1]),
          data[idx, meta$xvar], rep(0, length(data[idx, meta$xvar])),
          stroke = b$color, fill = b$color)
      } else {
        qdrawCircle(painter, data[idx, meta$xvar], rep(0, length(data[idx, meta$xvar])),
          r = b$size * data$.size[idx],
          stroke = b$color, fill = b$color)
      }
    }
    draw_brush(layer, painter, data, meta)
    message("here 13")
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
    meta$limits = extend_ranges(meta$limits, -sign(event$delta()) * 0.05)
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
      sprintf('row id: %s\n%s: %s\n%s: %s',
        paste(rownames(data)[idx], collapse = ', '),
        meta$xvar, paste(data[idx, meta$xvar], collapse = ', '))
    draw_identify(layer, painter, data, meta)
    if (meta$samesize) {
      qdrawGlyph(painter, qglyphCircle(r = 2 * b$size * data$.size[1]),
        data[idx, meta$xvar], rep(0, length(data[idx, meta$xvar])),
          stroke = b$color, fill = NA)
    } else {
      qdrawCircle(painter, data[idx, meta$xvar], rep(0, length(data[idx, meta$xvar])),
        r = b$size * meta$size, stroke = b$color, fill = NA)
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
  layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
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
  view$setWindowTitle(paste("Densityplot:", meta$xvar))
  meta$xvarChanged$connect(function() {
    view$setWindowTitle(paste("Densityplot:", meta$xvar))
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
      } else idx = c(1, 1, 2, 3, 3)[idx]
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
  sync_limits(meta, layer.main, layer.brush, layer.identify)

  ## simulate brushing
  meta$manual.brush = function(pos) {
    brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
  }
  message("here 0")

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
        breaks = 'numeric', asp = 'numeric', samesize = 'logical')
)))
