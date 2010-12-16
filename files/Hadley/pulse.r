# l(qtpaint)
# source("~/Documents/cranvas/demos/pulse.r")
library(qtpaint)
library(ggplot2)     
library(tourr, warn=F)

data <- rescaler(flea[1:2], "range")

view_size <- function(item) {
  qboundingRect(qtpaint:::qpaintingView(item))[2, ]
}


transp_cur <- rep(0.61, nrow(data))
transp_delta <- runif(nrow(data), 0.01, 0.05)
transp_dir <- 1

redraw <- function(item, painter, exposed) {
  circle <- qpathCircle(0, 0, max(min(view_size(item) / 20), 1))
  colours <- hcl(240, l = 100 * transp_cur)

  qstrokeColor(painter) <- NA
  # qdrawGlyph(painter, circle, data[,1], data[,2], fill = colours)
  
  for(i in 1:nrow(data)) {
    qfillColor(painter) <- colours[i]
    qdrawGlyph(painter, circle, data[i,1], data[i,2])
  }
}

scene <- qgraphicsScene()
root <- qlayer(scene)

points <- qlayer(root, redraw)
qlimits(points) <- qrect(c(-0.05, 1.05), c(-0.05, 1.05))

view <- qplotView(scene = scene)
print(view)

print(system.time({for(i in 1:50) {
  transp_dir <<- transp_dir * 
    ifelse(transp_cur > 0.9 | transp_cur < 0.6, -1, 1)
  transp_cur <<- transp_cur + transp_delta * transp_dir
  
  qupdate(scene)
  Sys.sleep(1/33)
}}))