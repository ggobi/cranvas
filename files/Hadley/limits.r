# source("~/Documents/cranvas/demos/limits.r")
library(plyr)

new_limits <- function(x, y) {
  ranges <- list(x = range(x, na.rm = TRUE), y = range(y, na.rm = TRUE))
  
  loc <- llply(ranges, mean)
  dim <- llply(ranges, diff)
  
  xlim <- function() loc$x + c(-1, 1) * dim$x / 2
  ylim <- function() loc$y + c(-1, 1) * dim$y / 2
  
  pan <- function(x_delta = 0, y_delta = 0) {
    loc$x <<- loc$x + x_delta
    loc$y <<- loc$y + y_delta
    self
  } 
  
  zoom <- function(delta) {
    dim$x <<- dim$x * delta
    dim$y <<- dim$y * delta
    self
  }
  
  zoom_in <- function() {
    zoom(0.70)
  }
  zoom_out <- function() {
    zoom(1 / 0.7)
  }
  
  set <- function(layer) {
    qlimits$setLimits(qrect(xlim(), ylim()))
    qupdate(layer)
    self
  }
  
  subset <- function(df) {
    base::subset(df, x >= xlim()[1] & x <= xlim()[2] & 
               y >= ylim()[1] & y <= ylim()[2])
  }
  
  self <- structure(list(
    xlim = xlim, ylim = ylim, set = set, subset = subset,
    pan = pan, 
    zoom = zoom, zoom_in = zoom_in, zoom_out = zoom_out
  ), class = "limits")
  self
}

print.limits <- function(x, ...) { 
  rng <- function(x) {
    x <- format(x, digits = 3, trim = TRUE)
    paste("(", x[1], ", ", x[2], ")", sep ="")
  }
  cat("Limits: ", rng(x$xlim()), " x ", rng(x$ylim()), "\n", sep ="")
}