#' Match keys from a keyboard event
#'
#' This is a simple wrapper function to test if the given keys are hit in the
#' keyboard event.
#'
#' @param key a character vector of key names (see the example below)
#' @param event the keyboard event (if missing, the default value comes from the
#'   \code{event} argument of the parent function (\code{sys.frame(1)}), so if
#'   this function is called under a standard callback of a layer event, we can
#'   leave this argument blank)
#' @return \code{TRUE} for the matched keys, and \code{FALSE} for those not
#'   matched
#' @author Yihui Xie <\url{http://yihui.name}>
#' @references \url{http://doc.qt.nokia.com/latest/qt.html#Key-enum}
#' @export
#' @examples library(qtbase)
#' library(qtpaint)
#' library(cranvas)
#' key_press = function(layer, event) {
#' print(match_key(c('A', 'F', 'PageUp', '1'), event))
#' }
#' s = qscene(); r = qlayer(s, keyPressFun = key_press)
#' qplotView(scene = s)
match_key = function(key, event) {
  if (missing(event)) event = get('event', sys.frame(1))  # get event from the callback
  k = event$key()
  e = attr(Qt$Qt, 'env')
  sapply(key, function(x) e[[sprintf('Key_%s', x)]] == k, USE.NAMES = FALSE)
}

ALL_KEYS = local({
  e = attr(Qt$Qt, 'env')
  keys = grep('^Key_', ls(e), value = TRUE)
  sapply(keys, function(i) e[[i]], simplify = FALSE)
})

## pass event$key() to me and I'll return you the key text
key2text = function(key) {
  keys = names(ALL_KEYS)
  for (k in keys) {
    if (ALL_KEYS[[k]] == key) return(sub('^Key_', '', k))
  }
  NULL
}

#' Some common processings in the keyboard and mouse events
#'
#' The key press and release events often involve with setting the selection
#' mode of the \code{\link{brush}}, the alpha transparency, and deleting
#' selected elements, and so on. Mouse press, release, move and hover are often
#' related to brushing and identifying cases. These functions implement these
#' common processes.
#'
#' @section Mouse Events: Left click to brush the plot with a rectangle brush,
#'   and right click to resize the brush (the cursor shape will become a cross).
#'   The middle button is used to toggle between two types of brushes: one type
#'   is to keep the brush on the plot when the mouse is released, and the other
#'   type is to hide it on mouse release. When the mouse is released, the brush
#'   history will be saved (\code{\link{save_brush_history}}).
#'
#' @section Key Events: The keys A, O, X, N and C corresponds to the selection
#'   mode AND, OR, XOR, NOT and COMPLEMENT respectively.
#'
#'   Plus (+) and Minus (-) can increase or decrease the alpha transparency
#'   exponentially.
#'
#'   The key Delete will make the selected elements invisible, and F5 makes all
#'   the elements visible.
#'
#'   The question key (?) toggles the identify mode (on or off). The cross
#'   cursor shape (+) indicates it is in the identify mode, and a normal cursor
#'   indicates the brush mode.
#'
#'   The key S acts like the middle button of the mouse (toggles between two
#'   brush types).
#'
#'   In a key release event, we set the selection mode to \code{'none'}. If
#'   PageUp or PageDown is pressed (or equivalently use square brackets \samp{[}
#'   and \samp{]}), we show the brush history step by step.
#' @rdname common_events
#' @param layer the layer argument in the event callback
#' @param event the event argument in the event callback
#' @param data the data created by \code{\link{qdata}}
#' @param meta the meta data for a plot
#' @return \code{NULL}
#' @author Yihui Xie <\url{http://yihui.name}>
#' @seealso \code{\link{brush}}
#' @export
#' @examples ## see the source code of qbar() or qparallel()
common_key_press = function(layer, event, data, meta) {
  if (length(i <- which(match_key(c('A', 'O', 'X', 'N', 'C'))))) {
    b = brush(data)
    b$mode = c('and', 'or', 'xor', 'not', 'complement')[i]
  } else if (length(i <- which(match_key(c('Plus', 'Minus'))))) {
    meta$alpha = max(0.01, min(1, c(1.1, 0.9)[i] * meta$alpha))
    data$.color = alpha(data$.color, meta$alpha)
    data$.border = alpha(data$.border, meta$alpha)
  } else if (match_key('Delete'))
    visible(data) = !selected(data) & visible(data) else if (match_key('F5'))
      visible(data) = TRUE
}
#' @rdname common_events
#' @export
common_key_release = function(layer, event, data, meta) {
  b = brush(data)
  b$mode = 'none'    # set brush mode to 'none' when release the key
  direction = which(match_key(c('PageUp', 'PageDown', 'BracketLeft', 'BracketRight')))
  if (length(direction)) {
    if (direction > 2L) direction = direction - 2L
    idx = b$history.index + c(-1, 1)[direction]
    idx = max(1, min(length(b$history.list), idx))
    b$history.index = idx
    selected(data) = b$history.list[[idx]]
  } else if (match_key('Question')) {
    b$identify = !b$identify
    b$cursor = if (b$identify) 2L else 0L
  } else if (match_key('S')) {
    b$select.only = !b$select.only
  }
}
#' @rdname common_events
#' @export
common_mouse_press = function(layer, event, data, meta) {
  b = brush(data)
  meta$start = as.numeric(event$pos())
  ## on right click, we can resize the brush; left click: only move the brush
  bt = event$button()
  if (length(i <- which(bt == c(Qt$Qt$LeftButton, Qt$Qt$RightButton)))) {
    if (b$select.only) {
      b$cursor = 2L; meta$brush.move = FALSE
      meta$brush.size = apply(meta$limits, 2, diff) / 100
    } else {
      b$cursor = c(0L, 2L)[i]; meta$brush.move = i == 1
    }
    b$draw.brush = TRUE
  } else if (bt == Qt$Qt$MidButton) {
    b$cursor = 2L; b$select.only = !b$select.only
  }
}
#' @rdname common_events
#' @export
common_mouse_move = function(layer, event, data, meta) {

}
#' @rdname common_events
#' @export
common_mouse_release = function(layer, event, data, meta) {
  b = brush(data)
  b$draw.brush = !b$select.only
  if (!b$select.only) b$cursor = 0L  # restore to Arrow cursor
  save_brush_history(data)  # store brushing history
}

common_focus_in = function(layer, event, data, meta) {
  meta$active = TRUE
}
common_focus_out = function(layer, event, data, meta) {
  meta$active = FALSE
}

key_layer = function(meta) {

  # a timer to remove the key hint after 2 secs
  tmr = qtimer(2000, function() meta$keys = character(0))

  l = qlayer(paintFun = function(layer, painter) {
    if (!length(meta$keys) || !nzchar(meta$keys)) return()
    lims = meta$limits
    qdrawText(painter, meta$keys, lims[2, 1], lims[2, 2], cex = 3,
              color = 'darkgray', halign = 'right', valign = 'top')
  }, keyPressFun = function(layer, event) {
    meta$keys = paste(c(meta$keys, key2text(event$key())), collapse = '+')
    tmr$start()
  }, limits = qrect(meta$limits))

  meta$keysChanged$connect(function() {
    qupdate(l)
  })
  l
}

#' Sync layer limits
#'
#' The limits information is stored in the meta data as \code{meta$limits}, of
#' which this function makes use to sync the limits of layers.
#'
#' An event is attached to \code{meta$limits} so that whenever it is changed,
#' the limits all the layers will be reset by the method
#' \code{layer$setLimits()}, hence we only need to take care of
#' \code{meta$limits} and this function will do the rest of work.
#'
#' Besides, the size and position of the brush will be restored.
#' @param meta the meta data contains a matrix of limits in \code{meta$limits}
#' @param ... an arbitrary number of layers
#' @return \code{NULL} (an event is attached on \code{meta$limits} so that
#'   whenever the limits are changed, the layers will be updated using the new
#'   limits)
#' @author Yihui Xie <\url{http://yihui.name}>
#' @note You do not need to call \code{\link[qtpaint]{qupdate}} to update the
#'   layers explicitly when \code{meta$limits} is changed, because
#'   \code{layer$setLimits()} will update the layers.
#' @export
#' @examples ## sync_limits(meta, layer1, layer2, layer3)
sync_limits = function(meta, ...) {
  l = list(...)
  l = l[!is.na(l)]
  meta$limitsChanged$connect(function() {
    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 30
    meta$pos = meta$limits[2:3]
    r = qrect(meta$limits)
    sapply(l, function(x) x$setLimits(r))
  })
}

# Is shift pressed in an event?
shift_on = function(event) event$modifiers() == Qt$Qt$ShiftModifier

#' Logical operations under different selection mode
#'
#' A selection mode is essentially a logical operation like AND, OR, and XOR,
#' etc.
#'
#' There are five selection modes: \describe{ \item{none}{ignore previous
#' selection and completely start over again} \item{and}{select the
#' intersection, i.e. the objects that are selected by two successive brushing
#' operations} \item{or}{select the union, i.e. any objects selected by all
#' previous operations and the current operation} \item{xor}{toggle the
#' selection} \item{not}{negation, i.e. exclude the objects under two successive
#' brushing operations} \item{complement}{the complement of the current
#' selection} } We can hold the key while brushing: A for 'and', O for 'or', X
#' for 'xor', N for 'not' and C for 'complement'.
#' @param x logical: the previous selection status
#' @param y logical: the current selection status (if \code{y} is a numeric
#'   vector, it will be converted to a logical vector of the same length with
#'   \code{x} with \code{TRUE}'s corresponding to the numeric indicies)
#' @param mode the selection mode string; see Details
#' @return a logical vector indicating whether the objects are selected
#' @author Yihui Xie <\url{http://yihui.name}>
#' @seealso \code{\link[base]{&}}, \code{\link[base]{|}},
#'   \code{\link[base]{xor}}, \code{\link[base]{!}}
#' @export
#' @examples
#' x1 = c(TRUE, TRUE, FALSE, FALSE)
#' x2 = c(FALSE, TRUE, TRUE, FALSE)
#' mode_selection(x1, x2, 'none')
#' mode_selection(x1, x2, 'and')
#' mode_selection(x1, x2, 'or')
#' mode_selection(x1, x2, 'xor')
#' mode_selection(x1, x2, 'not')
#' mode_selection(x1, x2, 'complement')
#'
#' mode_selection(x1, c(2, 3), 'and')  # equivalent to x2
mode_selection = function(x, y, mode = "none") {
  if (is.numeric(y)) {
    tmp = logical(length(x))
    tmp[y] = TRUE
    y = tmp
  }
  ## a series of logical operations
  ## if mode is not specified, return y, the current status
  switch(mode, none = y, and = x & y, or = x | y, xor = xor(x, y), not = x & !y,
         complement = !y, y)
}
