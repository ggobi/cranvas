# source("~/Documents/cranvas/demos/brush.r")
library(qtpaint)
library(ggplot2)

n <- 1e5
df <- data.frame(x = runif(n), y = runif(n))

bbase <- c(0,0)
h <- 0.2
w <- 0.2

bcolor <- "red"

view_size <- function(item) {
  qboundingRect(qtpaint:::qpaintingView(item))[2, ]
}

draw_points <- function(item, painter, exposed) {
  circle <- qpathCircle(0, 0, min(view_size(item)) / 200)
  qstrokeColor(painter) <- NA
  
  qfillColor(painter) <- alpha("black", 1/10)
  qdrawGlyph(painter, circle, df[, 1], df[, 2])
}

draw_brush <- function(item, painter, exposed) {
  qfillColor(painter) <- "white"
  qstrokeColor(painter) <- bcolor
  qlineWidth(painter) <- 2
  qdrawRect(painter, bbase[1], bbase[2], bbase[1]+w, bbase[2]-h)
}  

draw_brushed <- function(item, painter, exposed) {
  brect <- qrect(bbase[1], bbase[2], bbase[1]+w, bbase[2]-h)
  under_df <- df[qprimitives(points, brect),]
  circle <- qpathCircle(0, 0, min(view_size(item)) / 200)
  qstrokeColor(painter) <- NA
  qfillColor(painter) <- alpha(bcolor, 1/10)
  qdrawGlyph(painter, circle, under_df[,1], under_df[,2])
}

moveBrush <- function(event) {
  if (!is.null(drag_start)) {
    drag_end <- event$screenPos
    mat <- qdeviceMatrix(event$item, event$view)
    size <- qmap(mat, drag_end) - qmap(mat, drag_start)

    w <<- abs(size[1])
    h <<- abs(size[2])    
    
    pt <- drag_start
  } else {
    pt <- event$screenPos    
  }

  mat <- qdeviceMatrix(event$item, event$view)
  pt <- qmap(mat, pt)
  bbase <<- pt
  
  qupdate(brush)
}

drag_start <- NULL
start_drag <- function(event) {
  drag_start <<- event$screenPos 
}

end_drag <- function(event) {
  drag_start <<- NULL 
}


if (exists("view")) qclose(view)

scene <- qgraphicsScene()
root <- qlayer(scene)

view <- qplotView(scene = scene)

points <- qlayer(root, draw_points, 
  mouseMove = moveBrush, 
  mouseReleaseFun = end_drag, 
  mousePressFun = start_drag)
qlimits(points) <- qrect(range(df[,1]), range(df[,2]))

brush <- qlayer(root, draw_brush)
qcacheMode(brush) <- "none"
qlimits(brush) <- qlimits(points)

brushed <- qlayer(root, draw_brushed)
qcacheMode(brushed) <- "none"
qlimits(brushed) <- qlimits(points)

print(view)
