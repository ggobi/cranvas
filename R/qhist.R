#' Draw a histogram or a spine plot
#'
#' Draw an interactive histogram or spine plot based on a continuous variable,
#' optionally split by a categorical variable. It supports some common keyboard
#' interactions (see \code{\link{common_key_press}}) as well as other
#' interactions specific to histograms and spine plots.
#'
#' The splitting variable is usually specified in \code{\link{qdata}} as the
#' \code{color} or \code{border} argument; if it is present, each bar in the
#' plot will be split into categories.
#'
#' Arrow keys can be used to change the binwidth as well as the breakpoints in
#' the plot. Up and Down can increase and decrease the binwidth respectively;
#' Left and Right can move the breakpoints of the bins to the left (smaller) or
#' right (larger). Mouse wheel helps zoom in/out of the plot.
#'
#' In the identify mode, the breakpoints of the bin(s) as well as counts and
#' proportion of cases in the bin(s) are shown as text labels in the plot.
#'
#' The function \code{\link{qspine}} is a short-hand version of \code{qhist(...,
#' spine = TRUE)}.
#' @param x the name of the numeric variable to be used to draw the histogram or
#'   spine plot
#' @param bins the desired number of bins
#' @param binwidth the bin width (\code{range(x) / bins} by default)
#' @param freq draw the frequencies (\code{TRUE}) or densities (\code{FALSE})
#'   (only applies to histogram)
#' @param spine if \code{TRUE}, draw a spine plot (bar widths proportional to
#'   counts instead of being equal)
#' @param ... arguments passed to \code{\link{qhist}}
#' @inheritParams qbar
#' @return A histogram or a spine plot
#' @author Yihui Xie <\url{http://yihui.name}>
#' @export
#' @family plots
#' @example inst/examples/qhist-ex.R
qhist = function(x, data, bins = 30, binwidth = NULL, freq = TRUE, main = '',
                 horizontal = FALSE, spine = FALSE, xlim = NULL, ylim = NULL,
                 xlab = NULL, ylab = NULL) {
  data = check_data(data)
  b = brush(data)
  b$select.only = TRUE; b$draw.brush = FALSE  # a selection brush
  cueOn = FALSE
  meta = Hist.meta$new(
    var = as.character(as.list(match.call()[-1])$x), freq = freq, alpha = 1,
    horizontal = horizontal, main = main, active = TRUE, standardize = spine,
    spine = spine, multiplier = 1
  )
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
  compute_coords = function(reset = TRUE) {
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
    if (reset) {
      meta$xat = axis_loc(meta$breaks); meta$yat = axis_loc(c(0, meta$y))
      meta$xlabels = format(meta$xat)
      meta$ylabels = format(meta$yat)
      meta$xlab = if (is.null(xlab)) meta$var else xlab
      meta$ylab = if (is.null(ylab)) {
        if (meta$spine) 'Proportion' else if (meta$freq) 'Frequency' else 'Density'
      } else ylab
    }
    if (meta$spine) {
      meta$xright = cumsum(table(meta$value[idx])) / sum(idx)  # [0,1], prop counts
      meta$xleft = c(0, meta$xright[-meta$nlevel])
      if (reset) {
        meta$xat = c(0, meta$xright)
        meta$xlabels = unname(tapply(formatC(meta$breaks), meta$xat, function(x) {
          if (length(x) <= 1) x else {
            sprintf('%s%s(%s)', x[1], if (meta$horizontal) '' else '\n', x[length(x)])
          }
        }))
        meta$xat = unique(meta$xat)
      }
      meta$xleft = rep(meta$xleft, meta$nlevel2)
      meta$xright = rep(meta$xright, meta$nlevel2)
    } else {
      meta$xleft = rep(meta$breaks[-nb], meta$nlevel2)
      meta$xright = rep(meta$breaks[-1], meta$nlevel2)
    }
    meta$ybottom = c(cbind(0, tmp[, -meta$nlevel2, drop = FALSE])); meta$ytop = meta$y
    if (!freq && max(meta$ytop) < 0.5) {
      ## there seems to be an ugly bug in qt: rectangles with small height not drawn
      meta$multiplier = k = 1 / max(meta$ytop) * 10
      meta$ybottom = meta$ybottom * k; meta$ytop = meta$ytop * k
      if (reset) meta$yat = meta$yat * k; if (!is.null(ylim)) ylim = ylim * k
    }
    if (reset) meta$limits = extend_ranges(
      cbind(if (is.null(xlim)) range(c(meta$xleft, meta$xright)) else xlim,
            if (is.null(ylim)) range(c(meta$ybottom, meta$ytop)) else ylim)
    )
    meta$minor = ifelse(meta$spine, ifelse(meta$horizontal, 'x', 'y'), 'xy')
  }
  compute_coords()
  compute_colors = function() {
    .bar_compute_colors(data, meta)
  }
  compute_colors()
  flip_coords = function(bar.only = FALSE) {
    .bar_flip_coords(data, meta, bar.only)
  }
  flip_coords()
  meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15
  main_draw = function(layer, painter) {
    .bar_draw_main(layer, painter, meta)
  }
  brush_draw = function(layer, painter) {
    .bar_draw_brush(layer, painter, data, meta)
    if (meta$horizontal) {
      y0 = min(meta$ybottom); x0 = x1 = min(meta$xleft); y1 = max(meta$ytop)
    } else {
      x0 = min(meta$xleft); y0 = y1 = min(meta$ybottom); x1 = max(meta$xright)
    }
    qdrawSegment(painter, x0, y0, x1, y1) # draw a baseline
  }
  brush_mouse_press = function(layer, event) {
    common_mouse_press(layer, event, data, meta)
  }
  brush_mouse_move = function(layer, event) {
    rect = qrect(update_brush_size(meta, event))
    hits = layer$locate(rect)
    if (length(hits)) {
      hits = .find_intersect(meta$value, hits, meta$nlevel)
    }
    selected(data) = mode_selection(selected(data), hits, mode = b$mode)
    common_mouse_move(layer, event, data, meta)
  }
  brush_mouse_release = function(layer, event) {
    brush_mouse_move(layer, event)
    common_mouse_release(layer, event, data, meta)
  }
  mouse_wheel = function(layer, event) {
    pos = as.numeric(event$pos())
    lim = meta$limits
    p = (pos - lim[1, ]) / (lim[2, ] - lim[1, ])  # proportions to left/bottom
    s = if (horizontal) c(0, 0, p[2], 1 - p[2]) else c(p[1], 1 - p[1], 0, 0)
    meta$limits = extend_ranges(meta$limits, -sign(event$delta()) * 0.1 * s)
  }

  shift_anchor = function(shift) {
    brk = meta$breaks
    r = range(data[, meta$var], na.rm = TRUE, finite = TRUE)
    brk = brk + shift  # shift by +/-(2% bin)
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
    return(brk)
  }
  key_press = function(layer, event) {
    common_key_press(layer, event, data, meta)
    if (length(i <- which(match_key(c('Up', 'Down'))))) {
      meta$binwidth = c(1.05, 0.95)[i] * meta$binwidth  # larger/smaller bins
      if (meta$binwidth < ifelse(length(meta$binmin), meta$binmin, 1e-7)) {
        meta$binwidth = meta$binmin
        message('binwidth too small!')
      }
      initial_bins(default = FALSE)  # use new binwidth
      layer.cues$invalidateIndex()
      return()
    } else if (length(i <- which(match_key(c('Left', 'Right'))))) {
      shift = c(-1, 1)[i] * meta$binwidth / 50  # shift by +/-(2% bin)
      meta$breaks = shift_anchor(shift)
      layer.cues$invalidateIndex()
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
    k = .find_intersect(meta$value, idx, meta$nlevel)
    meta$identify.labels = sprintf(
      'bin: (%s]\ncount: %s\nproportion: %.2f%%',
      paste(meta$breaks[range(idx %% meta$nlevel) + c(1, 2)], collapse = ','),
      sum(k), mean(k) * 100
    )
    draw_identify(layer, painter, data, meta)
    idx = idx + 1
    qdrawRect(painter, meta$xleft[idx], meta$ybottom[idx], meta$xright[idx],
              meta$ytop[idx], stroke = b$color, fill = NA)
  }

  cue_mouse_move = function(layer, event) {
    pos = as.numeric(event$pos())
    eps = 2*pixelToXY(layer.main, meta$limits, 1,1)
    rect = qrect(pos[1]-eps[1], pos[2]-eps[2], pos[1]+eps[1], pos[2]+eps[2])
    hits = layer$locate(rect)
    if (length(hits)) {
      b$cursor = 18L # ClosedHandCursor
      if (hits[1]==0) {
        shift = pos[1] - meta$xleft[1]
        #          message(sprintf('anchor: %f', pos[1]))
        meta$breaks = shift_anchor(shift)
        layer.cues$invalidateIndex()
        qupdate(layer.cues)
        return()
      }
      if (hits[1]==1) {
        meta$binwidth = pos[1] - meta$xleft[1]  # larger/smaller bin width
        #          message(sprintf('binwidth: %f', meta$binwidth))
        if (meta$binwidth < ifelse(length(meta$binmin), meta$binmin, 1e-7)) {
          meta$binwidth = meta$binmin
          message('binwidth too small!')
        }
        initial_bins(default = FALSE)  # use new binwidth
        layer.cues$invalidateIndex()
        qupdate(layer.cues)
        return()
      }
    } else {
      # pass mouse move on, if cue is not being moved
      if (!cueOn) brush_mouse_move(layer.main, event)
    }
  }
  cue_hover = function(layer, event) {
    pos = as.numeric(event$pos())
    eps = 2*pixelToXY(layer.main, meta$limits, 1,1)
    rect = qrect(pos[1]-eps[1], pos[2]-eps[2], pos[1]+eps[1], pos[2]+eps[2])
    hits = layer.cues$locate(rect)
    #        meta$pos = as.numeric(event$pos())
    #        hits = layer.cues$locate(identify_rect(meta))
    if (length(hits > 0)) {
      # change cursor shape
      if (hits[1] == 0) b$cursor = 17L # OpenHandCursor # anchor
      if (hits[1] == 1) b$cursor = 17L # OpenHandCursor # binwidth
      if (hits[1] == 2) b$cursor = 5L # ArrowVertCursor # binheight

    } else {
      # pass hover on and reverse any changes to the cursor
      b$cursor = 2L # CrossCursor
      identify_hover(layer.main, event)
    }
  }
  pixelToXY = function(layer, limits, px, py) {
    dx = px/layer.main$geometry$width()*diff(range(limits[,1]))
    dy = py/layer.main$geometry$height()*diff(range(limits[,2]))
    c(dx,dy)
  }

  cue_draw = function(layer, painter) {
    ybottom = meta$limits[1,2]
    ytop = meta$limits[2,2]
    eps = pixelToXY(layer, meta$limits, 1,1)
    #print(ytop)
    anchorCue = c(meta$xleft[1]-eps[1], meta$xleft[1]+eps[1], 0.25*ybottom, 0.75*ybottom)
    binwidthCue = c(meta$xleft[2]-eps[1], meta$xleft[2]+eps[1], 0.25*ybottom, 0.75*ybottom)
    binheightCue = c(meta$limits[1,1], meta$limits[2,1], ytop-10*eps[2], ytop)
    qdrawRect(painter, anchorCue[1], anchorCue[3], anchorCue[2], anchorCue[4], stroke="grey50", fill="grey50")
    qdrawRect(painter, binwidthCue[1], binwidthCue[3], binwidthCue[2], binwidthCue[4], stroke="grey50", fill="grey50")
    color = rgb(t(col2rgb("grey50"))/255, alpha=0.2)
    qdrawRect(painter, binheightCue[1], binheightCue[3], binheightCue[2], binheightCue[4], stroke=color, fill=color)
  }
  cue_mouse_press = function(layer, event) {
    pos = as.numeric(event$pos())
    eps = 2*pixelToXY(layer, meta$limits, 1,1)
    rect = qrect(pos[1]-eps[1], pos[2]-eps[2], pos[1]+eps[1], pos[2]+eps[2])
    hits = layer$locate(rect)
    if (length(hits)) {
      cueOn <<- TRUE
      if (hits[1] == 2) { #adjust vertical height to current maximum bin height
        meta$limits[,2] =
          extend_ranges(c(meta$ybottom, meta$ytop))
        meta$yat = axis_loc(c(0, meta$limits[2,2]))
        meta$ylabels = format(meta$yat)
      }
    }
    common_mouse_press(layer.main, event, data, meta)
  }
  cue_mouse_release = function(layer, event) {
    if (cueOn) cueOn <<- FALSE
    else brush_mouse_release(layer.main, event)
  }

  scene = qscene()
  layer.root = qlayer(scene)
  layer.main = qlayer(
    paintFun = main_draw,
    mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_release,
    mouseMove = brush_mouse_move, hoverMoveFun = identify_hover,
    keyPressFun = key_press, keyReleaseFun = key_release, wheelFun = mouse_wheel,
    focusInFun = function(layer, event) {
      common_focus_in(layer, event, data, meta)
    }, focusOutFun = function(layer, event) {
      common_focus_out(layer, event, data, meta)
    },
    limits = qrect(meta$limits), clip = TRUE
  )
  layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
  layer.identify = qlayer(paintFun = identify_draw, limits = qrect(meta$limits))
  layer.cues = qlayer(
    paintFun = cue_draw,
    mousePressFun = cue_mouse_press, mouseReleaseFun = cue_mouse_release,
    mouseMove = cue_mouse_move, hoverMoveFun = cue_hover,
    keyPressFun = key_press, keyReleaseFun = key_release,
    focusInFun = function(layer, event) {
      common_focus_in(layer, event, data, meta)
    }, focusOutFun = function(layer, event) {
      common_focus_out(layer, event, data, meta)
    },
    limits = qrect(meta$limits)
  )
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
  layer.root[1, 2] = layer.cues
  layer.root[1, 3] = qlayer()
  set_layout = function() {
    fix_dimension(
      layer.root, row = list(
        id = c(0, 2, 3), value = c(prefer_height(meta$main),
                                   prefer_height(meta$xlabels),
                                   prefer_height(meta$xlab))),
      column = list(
        id = c(1, 0, 3), value = c(prefer_width(meta$ylabels),
                                   prefer_width(meta$ylab, FALSE),
                                   10))
    )
  }
  set_layout()
  meta$mainChanged$connect(set_layout)
  meta$xlabChanged$connect(set_layout); meta$ylabChanged$connect(set_layout)
  meta$xlabelsChanged$connect(set_layout); meta$ylabelsChanged$connect(set_layout);

  view = qplotView(scene = scene)
  view$setWindowTitle(paste(ifelse(meta$spine, "Spine plot:", "Histogram:"), meta$var))
  meta$varChanged$connect(function() {
    view$setWindowTitle(paste(ifelse(meta$spine, "Spine plot:", "Histogram:"), meta$var))
  })
  view$resize(480, 480)
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
  sync_limits(meta, layer.main, layer.brush, layer.identify, layer.cues)  # sync limits
  meta$manual.brush = function(pos) {
    brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
  }
  meta$breaksChanged$connect(function () {
    compute_coords(reset = FALSE); compute_colors(); flip_coords(bar.only = TRUE)
    layer.main$invalidateIndex()
    qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis); qupdate(layer.main)
  })
  attr(view, 'meta') = meta
  view$show()
}

Hist.meta = setRefClass("Hist_meta", fields = properties(c(

  Common.meta,

  list(var = 'character', value = 'factor', var2 = 'character', value2 = 'factor',
       x = 'numeric', y = 'numeric', breaks = 'numeric', horizontal = 'logical',
       xleft = 'numeric', xright = 'numeric', ybottom = 'numeric', ytop = 'numeric',
       split.type = 'character', spine = 'logical', nlevel = 'integer', nlevel2 = 'integer',
       freq = 'logical', standardize = 'logical', binwidth = 'numeric',
       multiplier = 'numeric', binmin = 'numeric')

)))
