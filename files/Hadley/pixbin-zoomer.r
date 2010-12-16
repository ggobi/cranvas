# source("~/Documents/cranvas/demos/pixbin-zoomer.r", chdir=T)
source("binning.r")

library(qtpaint)
library(reshape)
library(ash)

source("limits.r")
if (!exists("geo")) {
  load("geo.rdata")  
  df <- data.frame(y = geo$lat * 100, x = geo$long * 100)
  df <- df[complete.cases(df), ]
}

"dim.QViz::RLayer" <- function(item) {
  qboundingRect(qtpaint:::qpaintingView(item))[2, ]
}
pix_to_data <- function(data, layer) {
  data[, 2] <- ncol(layer) - data[, 2]
  mat <- qdeviceMatrix(layer, inverted = TRUE)
  qmap(mat, data)
}

scale01 <- function(x) (x - min(x)) / diff(range(x))

scatterplot <- function(layer, painter, exposed) {
  visible <- limits$subset(df)
  binned <- subset(pixbin2(visible$x, visible$y, dim(layer)), value > 0)
  coords <- pix_to_data(cbind(binned$X1, binned$X2), layer)
  
  col <- grey(scale01(-log(binned$value)))
  qantialias(painter) <- FALSE
  qdrawPoint(painter, coords[, 1], coords[, 2], stroke = col)
}

zoom <- function(ev) {
  if (ev$delta > 0) {
    limits$zoom_in()
    qupdate(points)
  } else {
    limits$zoom_out()
    qupdate(points)
  }
}

scene <- qscene()
root <- qlayer(scene)

points <- qlayer(root, scatterplot, wheelFun = zoom)
points$setLimits(qrect(c(0, 1), c(0, 1)))

limits <- new_limits(df$x, df$y)

view <- qplotView(scene = scene, opengl = FALSE)
print(view)