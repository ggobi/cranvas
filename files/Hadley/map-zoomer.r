# source("~/Documents/cranvas/demos/map-zoomer.r", chdir= T)
library(qtpaint)
library(ggplot2)

if (!exists("geo")) {
  load("geo.rdata")  
}
df <- data.frame(y = geo$lat * 100, x = geo$long * 100)
df <- df[complete.cases(df), ]

scatterplot <- function(layer, painter, exposed) {
  qstrokeColor(painter) <- "black"
  qfillColor(painter) <- "black"
  if (zoom_level > 10) {
    circle <- qpathCircle(0, 0, 2)    
    qdrawGlyph(painter, circle, df$x, df$y)    
  } else {
    qdrawPoint(painter, df$x, df$y)    
  }
}

midmean <- function(x) mean(range(x, na.rm = TRUE))
half_range <- function(x) diff(range(x, na.rm = TRUE)) / 2

pos <- c(midmean(df$x), midmean(df$y))
rng <- c(half_range(df$x), half_range(df$y))
zoom_level <- 0

mouse_zoom <- function(event) {
  pos <<- event$pos
  if (event$modifiers[["control"]]) {
    zoom_out()
  } else {
    zoom_in()
  }
}

zoom_in <- function(event, ...) {  
  zoom_level <<- zoom_level + 1
  zoom_update()
}
zoom_out <- function() {
  zoom_level <<- zoom_level - 1
  zoom_update()  
}
zoom_update <- function() {
  qlimits(points) <- qrect(
    c(pos[1] - rng[1] / 1.4 ^ zoom_level, pos[1] + rng[1] / 1.4 ^ zoom_level), 
    c(pos[2] - rng[2] / 1.4 ^ zoom_level, pos[2] + rng[2] / 1.4 ^ zoom_level)
  )
}

handle_keys <- function(event) {
  if (event$key == "up") {
    pos[2] <<- pos[2] + rng[2] / zoom_level / 8
  } else if (event$key == "down") {
    pos[2] <<- pos[2] - rng[2] / zoom_level / 8
  } else if (event$key == "left") {
    pos[1] <<- pos[1] - rng[1] / zoom_level / 8
  } else if (event$key == "right") {
    pos[1] <<- pos[1] + rng[1] / zoom_level / 8
  }
  zoom_update()
  qupdate(scene)
}  

scene <- qgraphicsScene()
points <- qlayer(scene, scatterplot, 
  mouseDoubleClickFun = mouse_zoom, keyPressFun = handle_keys)
zoom_update()

view <- qplotView(scene = scene)
print(view)