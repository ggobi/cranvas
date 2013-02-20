#' 2D histograms
#'
#' Bin points into hexegons or rectangles.
#' @export
qhist2d = function(x, y, data, method = c('hex', 'rect'), ...) {
  switch(match.arg(method), hex = hist2d_hex, rect = hist2d_rect)(
    x, y, data, ...
  )
}

# print() or plot() method on a cranvas object
print.cranvas = plot.cranvas = function(x) {
  scene = qscene()
  root = qlayer(scene)
  for (r in x$layers) {
    root[r$pos[1], r$pos[2]] = r$layer
  }
  print(qplotView(scene = scene))
}

`+.cranvas` = function(x, y) {
  y$layer$setLimits(qrect(x$limits))
  x$layers[[length(x$layers) + 1]] = y
  x
}

hist2d_hex = function(x, y, data, xlim = NULL, ylim = NULL) {
  library(densityvis)
  z = as.list(match.call()[-1])
  meta = Hist2d.meta$new(xvar = as.character(z$x), yvar = as.character(z$y))
  bin = hex_bin(data[, meta$xvar], data[, meta$yvar])
  col = cscale(bin$freq, seq_gradient_pal(low = "grey95", high = "black"))
  hexes = hex_coord(bin$x, bin$y, attr(bin, "width"), attr(bin, "height"))
  meta$limits = extend_ranges(cbind(
    if (is.null(xlim)) range(bin$x) else xlim,
    if (is.null(ylim)) range(bin$y) else ylim
  ))

  main_draw = function(layer, painter) {
    qdrawPolygon(painter, hexes[, 1], hexes[, 2], fill = col)
  }

  layer.main = make_layer(
    paintFun = main_draw,
    limits = qrect(meta$limits),
    pos = c(1, 2)
  )
  structure(list(layers = list(layer.main), limits = meta$limits), class = 'cranvas')
}

# create a layer with position info in the grid layout
make_layer = function(..., pos) {
  list(layer = qlayer(...), pos = pos)
}

layer_point = function(x, y, data) {
  z = as.list(match.call()[-1])
  xvar = as.character(z$x); yvar = as.character(z$y)
  make_layer(paintFun = function(layer, painter) {
    qdrawCircle(painter, data[, xvar], data[, yvar], r = 2, stroke=NA, fill = rgb(0,0,0,.5))
  }, pos = c(1, 2))
}

hist2d_rect = function(x, y, data, xlim = NULL, ylim = NULL) {
  library(densityvis)
  z = as.list(match.call()[-1])
  meta = Hist2d.meta$new(xvar = as.character(z$x), yvar = as.character(z$y))
  bin = rect_bin(data[, meta$xvar], data[, meta$yvar])
  col = cscale(bin$count, seq_gradient_pal(low = "grey95", high = "black"))
  meta$limits = extend_ranges(cbind(
    if (is.null(xlim)) range(c(bin$left, bin$right)) else xlim,
    if (is.null(ylim)) range(c(bin$bottom, bin$top)) else ylim
  ))

  main_draw = function(layer, painter) {
    qdrawRect(painter, bin$left, bin$bottom, bin$right, bin$top, fill = col)
  }

  scene = qscene()
  layer.root = qlayer(scene)
  layer.main = qlayer(
    paintFun = main_draw,
    limits = qrect(meta$limits)
  )
  layer.root[1, 2] = layer.main
  qplotView(scene = scene)

}

Hist2d.meta = setRefClass(
  "Hist2d_meta",
  fields = properties(c(

    Common.meta,

    list(xvar = 'character', yvar = 'character')

  ))
)
