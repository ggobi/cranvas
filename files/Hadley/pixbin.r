# source("~/Documents/cranvas/demos/pixbin.r", chdir = T)
source("binning.r")
library(qtpaint)
library(ash)

library(ggplot2)

# If the fineness (range / resolution) of the data is small, then the data
# is somewhat discrete. When pixel-binning this gives distracting streaks 
# where there is no data. We resolve this by jittering the data to decrease
# the resolution to effectively zero. 
fineness <- function(x) diff(range(x, na.rm = TRUE)) / resolution(x)
jitter2 <- function(x) {
  half_res <- resolution(x) / 2
  x + runif(length(x), -half_res, half_res)
}

df <- data.frame(x = jitter2(diamonds$carat), y = diamonds$price)

"dim.QViz::RLayer" <- function(item) {
  qboundingRect(qtpaint:::qpaintingView(item))[2, ]
}
pix_to_data <- function(data, layer) {
  mat <- qdeviceMatrix(layer, inverted = TRUE)
  qmap(mat, data)
}
data_to_pix <- function(data, layer) {
  mat <- qdeviceMatrix(layer, inverted = FALSE)
  qmap(mat, data)  
}

scale01 <- function(x) (x - min(x)) / diff(range(x))

scatterplot <- function(layer, painter, exposed) {
  binned <- pixbin3(df$x, df$y, dim(layer))
  coords <- pix_to_data(cbind(binned$X1, binned$X2), layer)
  
  col <- grey(scale01(-log10(binned$value)))
  qantialias(painter) <- FALSE
  qdrawPoint(painter, coords[, 1], coords[, 2], stroke = col)
}

scene <- qscene()
root <- qlayer(scene)

points <- qlayer(root, scatterplot)
points$setLimits(qrect(range(df$x), range(df$y)))

view <- qplotView(scene = scene)
print(view)