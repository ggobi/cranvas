#' Draw a parallel coordinates plot
#'
#' This function creates a parallel coordinates plot (par-coords) for variables
#' in a data, with each line representing a row.
#'
#' See \code{\link{common_key_press}} for a series of common interactions.
#' Interactions specific to par-coords include: press \code{R} to toggle the
#' min/max labels; the arrow keys are used to adjust the order of the variables
#' and flip the values of variables (like a mirror reflection) when the axes are
#' selected by the brush.
#' @inheritParams qmval
#' @inheritParams qbar
#' @param scale data standardizing method; possible values are \code{'range'}
#'   (scale columns individually to [0, 1]), \code{'I'} (do not transform; use
#'   original values), \code{'sd'} (make each column of mean 0 sd 1), and
#'   \code{'global'} (scale all the columns to [0, 1] using global minimum and
#'   maximum); other character strings here means to use custom functions (see
#'   examples below)
#' @param names the variable labels to use in the plot (by default, they are the
#'   variable names with non-alphanumeric characters replaced by line breaks
#'   \code{'\n'})
#' @param na.action the function to deal with missing values
#' @param center the function to calculate where to center all the variables
#'   (e.g. center at the medians), or a numeric value, or \code{NULL} (do not
#'   center)
#' @param order methods to reorder the variables; see \code{\link{reorder_var}}
#' @param horizontal logical: direction of axes (horizontal or vertical)
#' @param glyph draw complete segments for all observations or other types of
#'   glyphs to represent observations (the latter has speed gain in case of
#'   large data)
#' @param boxplot logical: overlay boxplots on top of the par-coords plot or not
#' @param width width of boxplots
#' @param jitter \code{NULL} (no jittering) or a character vector to jitter
#'   variables (usually those categorical variables)
#' @param amount jitter amount
#' @param main title of plot
#' @param alpha value for alpha-blending
#' @return A par-coords plot
#' @author Yihui Xie
#' @export
#' @family plots
#' @example inst/examples/qparallel-ex.R
qparallel = function(
  vars = ~., data, scale = 'range', names = break_str(vars), na.action = na_impute,
  center = NULL, order = c('none', 'MDS', 'ANOVA', 'randomForest'), horizontal = FALSE,
  glyph = c('auto', 'line', 'tick', 'circle', 'square', 'triangle'),
  boxplot = FALSE, width = NULL, jitter = NULL, amount = NULL, main = '', alpha = 1
) {

  data = check_data(data)
  b = brush(data)    # the brush attached to the data

  vars = var_names(vars, data)
  if (length(vars) <= 1L)
    stop('parallel coordinate plots need at least 2 variables!')

  # meta data used to store useful information
  meta = Parallel.meta$new(
    brush.move = TRUE, alpha = alpha, active = TRUE, main = main, vars = vars,
    glyph = match.arg(glyph), order = match.arg(order), draw.range = FALSE,
    horizontal = horizontal, jitter = jitter, amount = amount, names = names
  )

  data_preprocess = function() {
    tmp = as.data.frame(data[, meta$vars], stringsAsFactors = TRUE)
    tmp = .rm.cons.col(tmp)  # remove constant columns
    if (length(meta$vars <- names(tmp)) <= 1)
      stop('there are less than 2 variables in the data')
    # which columns are numeric? we don't want boxplots for non-numeric vars
    meta$numeric.col = sapply(tmp, is.numeric)
    names(meta$numeric.col) = names(meta$names) = meta$vars
    meta$plot.data = sapply(tmp, as.numeric)
    meta$p = ncol(meta$plot.data)
    meta$n = nrow(meta$plot.data)

    meta$plot.data = na.action(meta$plot.data) # handle missing values

    if (length(meta$jitter)) {
      meta$plot.data[, meta$jitter] = apply(
        meta$plot.data[, meta$jitter, drop = FALSE], 2, base::jitter,
        amount = if (length(meta$amount)) meta$amount
      )  # jittering
    }

    scale = switch(
      scale, range = rescale_range, sd = rescale_sd, I = identity,
      global = function(x) {
        (x - min(meta$plot.data)) / diff(range(meta$plot.data, na.rm = TRUE))
      }, get(scale)
    )
    meta$plot.data = apply(meta$plot.data, 2, scale)  # standardizing

    if (!is.null(center)) {
      meta$plot.data = apply(meta$plot.data, 2, function(x) {
        x - ifelse(is.function(center), center(x), as.numeric(center))  # centering
      })
    }
  }

  # final calculations for graphical primitives, e.g. segments, axis labels
  data_primitives = function() {
    # switch x and y according to the direction
    idx = visible(data)
    meta$x = col(meta$plot.data); meta$y = meta$plot.data
    meta$xat = 1:meta$p; meta$xlabels = meta$names
    meta$yat = axis_loc(meta$y); meta$ylabels = format(meta$yat)
    lims = extend_ranges(matrix(c(range(meta$x[idx, ]), range(meta$y[idx, ])), 2))
    if (!identical(lims, meta$limits) && !meta$horizontal) meta$limits = lims
    if (meta$horizontal) {
      switch_value('x', 'y', meta)
      switch_value('xat', 'yat', meta)
      switch_value('xlabels', 'ylabels', meta)
      switch_value('xlab', 'ylab', meta)
      if (!identical(meta$limits, lims[, 2:1])) meta$limits = lims[, 2:1]
    }
    # 'auto' means 'line's when n*p<=5000*10, and 'tick's otherwise
    if (meta$glyph == 'auto')
      meta$glyph = ifelse(meta$n * meta$p <= 50000, 'line', 'tick')
    if (meta$glyph == 'line') {
      # creating starting and ending vectors, because indexing in real-time is slow
      meta$segx0 = as.vector(t.default(meta$x[, -meta$p]))
      meta$segx1 = as.vector(t.default(meta$x[, -1]))
      meta$segy0 = as.vector(t.default(meta$y[, -meta$p]))
      meta$segy1 = as.vector(t.default(meta$y[, -1]))
    } else {
      meta$x0 = as.vector(t.default(meta$x))
      meta$y0 = as.vector(t.default(meta$y))
    }
    if (boxplot) {
      # automatic box width
      meta$width = if (is.null(width)) max(1/meta$p, 0.2) else width
      meta$at = which(meta$numeric.col)
      bxp.data = lapply(
        as.data.frame(meta$plot.data[idx, meta$numeric.col]), boxplot.stats,
        do.conf = FALSE
      )
      meta$bxp.stats = sapply(bxp.data, `[[`, 'stats')
    }
    meta$minor = ifelse(meta$horizontal, 'y', 'x')
  }

  # given orders, rearrange the data
  # need to update: numcol, plot_data, vars, boxplot data, primitives data
  data_reorder = function(vars) {
    meta$numeric.col = meta$numeric.col[vars]
    meta$names = meta$names[vars]
    meta$plot.data = meta$plot.data[, vars]
    meta$vars = colnames(meta$plot.data)
    data_primitives()
  }

  # do the transformation now
  data_preprocess()

  # order by MDS or ANOVA
  data_reorder(reorder_var(data = meta$plot.data, type = meta$order, vars = meta$vars,
                           numcol = meta$numeric.col, x = data$.color))

  # brush range: horizontal and vertical
  meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15

  draw.glyph = switch(
    meta$glyph, tick = qglyphSegment(direction = ifelse(horizontal, pi/2, 0)),
    circle = qglyphCircle(), square = qglyphSquare(),
    triangle = qglyphTriangle()
  )

  # par-coords segments
  main_draw = function(layer, painter) {
    .color = data$.color;
    if (meta$alpha < 1) {
      .color = rgb(t(col2rgb(.color))/255, alpha = meta$alpha)
    }
    .color[!visible(data)] = NA

    if (meta$glyph == 'line') {
      segcol = rep(.color, each = meta$p - 1)
      qdrawSegment(painter, meta$segx0, meta$segy0, meta$segx1, meta$segy1,
                   stroke = segcol)
    } else {
      main.col = rep(.color, each = meta$p)
      .border = data$.border; .border[!visible(data)] = NA
      .border = rep(.border, each = meta$p)
      qdrawGlyph(painter, draw.glyph, meta$x0, meta$y0, fill = main.col,
                 stroke = .border)
    }
  }

  # annotate maximum and minimum values for each axis
  range_draw = function(layer, painter) {
    if (!meta$draw.range) return()
    if (any(meta$numeric.col)) {
      dat = as.data.frame(data)[, meta$vars][, meta$numeric.col]
      range.d = round(as.matrix(apply(dat, 2, range, na.rm=TRUE)), 2)
      numcol = which(meta$numeric.col)
      if (!meta$horizontal) {
        qdrawText(painter, range.d[1, ], numcol, meta$limits[3], valign = 'bottom')
        qdrawText(painter, range.d[2, ], numcol, meta$limits[4], valign = 'top')
      } else {
        qdrawText(painter, range.d[1, ], meta$limits[1], numcol, halign = 'left')
        qdrawText(painter, range.d[2, ], meta$limits[2], numcol, halign = 'right')
      }
    }
  }

  # record the coordinates of the mouse on click
  brush_mouse_press = function(layer, event) {
    common_mouse_press(layer, event, data, meta)
  }

  # monitor keypress event
  brush_key_press = function(layer, event) {
    # common key press processings
    common_key_press(layer, event, data, meta)
    # whether to draw min/max labels
    if (match_key('R')) {
      meta$draw.range = !meta$draw.range
      qupdate(layer.range)
      return()
    }
    i = which(match_key(c('Left', 'Right', 'Down', 'Up')))
    if (length(i) && length(meta$pos)) {
      if (!meta$horizontal) {
        j = 1
        movedir = switch(i, -1, 1, NULL, NULL)
        flipdir = switch(i, NULL, NULL, -1, 1)
      } else {
        j = 2
        movedir = switch(i, NULL, NULL, -1, 1)
        flipdir = switch(i, -1, 1, NULL, NULL)
      }
      xs = discard(1:meta$p, sort(meta$pos[j] - c(0, 1) * meta$brush.size[j]))
      if ((nxs <- length(xs))) {
        update.all = FALSE
        if (!is.null(movedir)) {
          vars0 = meta$vars
          if (xs[1] > 1 & movedir == -1) {
            vars0[c(xs[1] - 1, xs)] = vars0[c(xs, xs[1] - 1)]
            meta$pos[j] = meta$pos[j] - 1
          }
          if (xs[nxs] < meta$p & movedir == 1) {
            vars0[c(xs, xs[nxs] + 1)] = vars0[c(xs[nxs] + 1, xs)]
            meta$pos[j] = meta$pos[j] + 1
          }
          if (any(vars0 != meta$vars)) {
            data_reorder(vars0); update.all = TRUE
          }
        }
        if (!is.null(flipdir)) {
          meta$plot.data[, xs] = apply(
            meta$plot.data[, xs, drop = FALSE], 2,
            function(xx) {
              meta$limits[c(3,1)[j]] + meta$limits[c(4,2)[j]] - xx
            }
          )
          data_primitives(); update.all = TRUE
        }
        if (update.all) {
          qupdate(layer.xaxis); qupdate(layer.yaxis)
          layer.main$invalidateIndex()
          qupdate(layer.main); qupdate(layer.brush); qupdate(layer.boxplot)
        }
      }
    }
  }
  brush_key_release = function(layer, event) {
    common_key_release(layer, event, data, meta)
  }

  # identify segments being brushed when the mouse is moving
  brush_mouse_move = function(layer, event) {
    rect = qrect(update_brush_size(meta, event))
    hits = layer$locate(rect) + 1
    # ticks and lines are of different numbers!
    hits = ceiling(hits/ifelse(meta$glyph == 'line', meta$p - 1, meta$p))
    selected(data) = mode_selection(selected(data), hits, mode = b$mode)
    common_mouse_move(layer, event, data, meta)
  }
  brush_mouse_release = function(layer, event) {
    brush_mouse_move(layer, event)
    common_mouse_release(layer, event, data, meta)
  }

  # convert a matrix to coordinates of segments
  mat2seg = function(x, idx = 1:nrow(x)) {
    x = x[idx, , drop = FALSE]
    as.vector(t.default(cbind(x, NA)))
  }

  # draw the segments under the brush with another appearance
  brush_draw = function(layer, painter) {
    .visible = which(visible(data))
    if (b$persistent && length(b$persistent.list)) {
      qlineWidth(painter) = b$size
      for (i in seq_along(b$persistent.list)) {
        idx = intersect(b$persistent.list[[i]], .visible)
        if (!length(idx)) next
        qstrokeColor(painter) = b$persistent.color[i]
        tmpx = mat2seg(meta$x, idx)
        tmpy = mat2seg(meta$y, idx)
        nn = length(tmpx)
        qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
      }
    }
    .brushed = intersect(which(selected(data)), .visible)
    if (length(.brushed)) {
      qlineWidth(painter) = b$size
      qstrokeColor(painter) = b$color
      tmpx = mat2seg(meta$x, .brushed)
      tmpy = mat2seg(meta$y, .brushed)
      nn = length(tmpx)
      qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
    }
    draw_brush(layer, painter, data, meta)
  }

  identify_hover = function(layer, event) {
    if (!b$identify) return()
    b$cursor = 2L  # Cross
    meta$pos = as.numeric(event$pos())
    hits = layer$locate(identify_rect(meta)) + 1
    meta$identified = ceiling(hits/ifelse(meta$glyph == 'line', meta$p - 1, meta$p))
    meta$identified = unique(meta$identified)
    qupdate(layer.identify)
  }

  identify_draw = function(layer, painter) {
    if (b$identify && length(meta$identified)) {
      qlineWidth(painter) = b$size
      qstrokeColor(painter) = b$color
      tmpx = mat2seg(meta$x, meta$identified)
      tmpy = mat2seg(meta$y, meta$identified)
      nn = length(tmpx)
      qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
      # identify labels
      meta$identify.labels = b$label.gen(data[meta$identified, meta$vars])
      draw_identify(layer, painter, data, meta)
    }
  }

  scene = qscene()
  layer.root = qlayer(scene)

  layer.main = qlayer(
    paintFun = main_draw,
    mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_release,
    mouseMoveFun = brush_mouse_move, keyPressFun = brush_key_press,
    keyReleaseFun = brush_key_release, hoverMoveFun = identify_hover,
    focusInFun = function(layer, event) common_focus_in(layer, event, data, meta),
    focusOutFun = function(layer, event) common_focus_out(layer, event, data, meta),
    limits = qrect(meta$limits), cache = TRUE
  )

  layer.range = qlayer(paintFun = range_draw, limits = qrect(meta$limits))
  layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
  layer.identify = qlayer(paintFun = identify_draw, limits = qrect(meta$limits))
  layer.title = qmtext(meta = meta, side = 3)
  layer.xaxis = qaxis(meta = meta, side = 1)
  layer.yaxis = qaxis(meta = meta, side = 2)
  layer.grid = qgrid(meta = meta)
  layer.legend = qlayer()  # legend layer (currently only acts as place holder)

  layer.root[0, 1] = layer.title
  layer.root[2, 1] = layer.xaxis
  layer.root[1, 0] = layer.yaxis
  layer.root[1, 1] = layer.grid

  layer.boxplot = if (boxplot) {
    qbxp(data, meta, limits = qrect(meta$limits))
  } else qlayer()
  layer.root[1, 1] = layer.boxplot

  layer.root[1, 1] = layer.main
  layer.root[1, 1] = layer.range
  layer.root[1, 1] = layer.brush
  layer.root[1, 1] = layer.identify
  layer.root[1, 2] = layer.legend

  # update the brush layer in case of any modifications to the mutaframe
  d.idx = add_listener(data, function(i, j) {
    switch(
      j, .brushed = qupdate(layer.brush),
      .color = qupdate(layer.main), {
        data_preprocess(); data_primitives()
        qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
        layer.main$invalidateIndex(); qupdate(layer.main)
      }
    )
  })

  set_layout = function() {
    fix_dimension(
      layer.root,
      row = list(id = c(0, 2), value = c(prefer_height(meta$main),
                                         prefer_height(meta$xlabels))),
      column = list(id = c(0, 2), value = c(prefer_width(meta$ylabels), 10)))
  }
  set_layout()
  meta$mainChanged$connect(set_layout)
  meta$xlabelsChanged$connect(set_layout); meta$ylabelsChanged$connect(set_layout);

  view = qplotView(scene = scene)
  view$setWindowTitle(sprintf('Par-coords plot: %s', paste(meta$vars, collapse = ', ')))
  meta$varsChanged$connect(function() {
    view$setWindowTitle(sprintf('Par-coords plot: %s', paste(meta$vars, collapse = ', ')))
  })
  view$resize(480 * sqrt(length(meta$vars)/3), 480)

  # update the brush layer if brush attributes change
  b.idx = b$colorChanged$connect(function() {
    qupdate(layer.brush)
  })

  qconnect(layer.main, 'destroyed', function(x) {
    b$colorChanged$disconnect(b.idx)
    remove_listener(data, d.idx)
  })
  # change the cursor
  b$cursorChanged$connect(function() {
    set_cursor(view, b$cursor)
  })
  # more attributes to come
  sync_limits(meta, layer.main, layer.brush, layer.identify, layer.range, layer.boxplot)
  meta$manual.brush = function(pos) {
    brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
  }
  attr(view, 'meta') = meta
  view
}

Parallel.meta = setRefClass(
  'Parallel_meta', contains = 'CommonMeta',
  fields = properties(list(
    vars = 'character', glyph = 'character', names = 'character',
    order = 'character', draw.range = 'logical',
    plot.data = 'matrix', numeric.col = 'logical',
    p = 'numeric', n = 'numeric', horizontal = 'logical',
    jitter = 'character', amount = 'numeric', x = 'matrix', y = 'matrix',
    segx0 = 'numeric', segx1 = 'numeric', segy0 = 'numeric', segy1 = 'numeric',
    x0 = 'numeric', y0 = 'numeric', at = 'numeric',
    width = 'numeric', bxp.stats = 'matrix', bxp.out = 'list'
  ))
)
