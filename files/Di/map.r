# setwd("/Users/dicook/cranvas/code/files/Di")
# load(".Rdata")
# source("/Users/dicook/cranvas/code/files/Di/map.r")
# source("/Users/dicook/cranvas/code/R/optimization.R")
# source("/Users/dicook/cranvas/code/R/interaction.R")
# source("/Users/dicook/cranvas/code/R/data.R")

library(qtbase)
library(qtpaint)
library(plumbr)

# st<-read.csv("/Users/dicook/cranvas/code/files/Di/map-sample.csv")
# st.all<-read.csv("/Users/dicook/cranvas/code/files/Di/cartogram-polygons.csv")
# qmap(st)
# qmap(st.all, id="State", x="Longitude", y="Latitude")
# qst.all <- qmutaframe(st.all, .brushed=FALSE)
# qmap(qst.all, id="State", x="Longitude", y="Latitude")
# qmap(qst.all, id="State", x="Longitude", y="Latitude", label="State")
qmap<-function(data, id="id", x="x", y="y", label="label", vars, ...) {

  #windowRanges <- make_window_ranges(ranges, xlab, ylab)
  #cat(id, x, y, is.character(id), "\n")
  data.map <- data.frame(id=data[,id], x=data[,x], y=data[,y], label=data[,label])

  xr <- range(data.map$x)
  yr <- range(data.map$y)
#  lims <- qrect(xr[1]-0.1*(xr[2]-xr[1]), yr[1]-0.1*(yr[2]-yr[1]), xr[1]+0.1*(xr[2]-xr[1]),yr[1]+0.1*(yr[2]-yr[1]))
  lims <- qrect(0, 0, 500, 500)
  #cat(xr[1], xr[2], yr[1], yr[2], "\n")

  scene = qscene()

#  bglayer = qlayer(scene, coords, limits = lims, clip = FALSE
#		# , keyPressFun=keyPressFun
#  )

####################################################
# Identify, using hover code (Barret's)
  .map_queryPos <- NULL
#  .map_hover_section <- list(top = -1, bottom = 1, right = -1, left = 1)
  map_hover_draw <- function(item, painter, exposed, ...) {
     cat("\nMap Hover Draw\n")
     # Don't draw when brushing
#    if (is.null(.map_queryPos)) return()

#    x <- .map_queryPos[2]
#    y <- .map_queryPos[1]

#    section <- subset(maps_info$data, (y <= top) & (y >= bottom) & (x <= right) & (x >=left))

    # Nothing under mouse
#    if (nrow(section) == 0) {
#      .map_hover_section <<- list(top = -1, bottom = 1, right = -1, left = 1)
#      return()
#    }


    # Highlight the polygon
#    brushColor <- brush_attr(mf_data, ".brushed.color")
#    qdrawRect(painter,
#      xleft = c(section$bottom), #left
#      ybottom = c(section$right), # bottom
#      xright = c(section$top), # right
#      ytop = c(section$left), # top
#      stroke = brushColor,
#      fill = c(NA)# fill
#    )

    # Work out label text
#    infostring <- paste("\nbin:", section[1,"label"], sep = " ")

#    qstrokeColor(painter) <- "white"
    qdrawText(painter, infostring, .map_queryPos[1], .map_queryPos[2], valign="top", halign="left")

#    .map_hover_section <<- list(top = section$top, bottom = section$bottom, left = section$left, right = section$right)
  }

  map_hover <- function(item, event, ...) {
  # if (.brush) return()

  .map_queryPos <<- as.numeric(event$pos())
  # qupdate(querylayer)

  cat("\nMap Hover\n")

  x <- .map_queryPos[2]
  y <- .map_queryPos[1]

  cat("x: ", x, "\ty: ", y, "\n")

#    if ( !((y <= .map_hover_section$top) & (y >= .map_hover_section$bottom) & (x <= .map_hover_section$right) & (x >= .map_hover_section$left))) {
    qupdate(hoverlayer)
  }

  polygon_leave <- function(item, event, ...) {
  # if (.brush) return()
  #
  # .map_queryPos <<- as.numeric(event$pos())
  # qupdate(querylayer)
    cat("\nMap Leave\n")
#    print(as.numeric(event$pos()))
    qupdate(hoverlayer)
  }

  mapdraw <- function(item, painter) {
    npolys<-length(unique(data.map$id))
    polyids<-unique(data.map$id)
    #cat("num states ",npolys, "\n")
    for (i in 1:npolys) {
      #cat("states ", i, polyids[i], "\n")
      sub <- subset(data.map, id == polyids[i])
      #cat("sub ",sub[1,1], sub[1,2], sub[1,3], nrow(sub), "\n")
      x<-round((sub$x-xr[1])/(xr[2]-xr[1])*450+25,0)
      y<-round((sub$y-yr[1])/(yr[2]-yr[1])*450+25,0)
      qdrawPolygon(painter, x, y, stroke="black", fill="grey")
    }
  }
  datalayer = qlayer(scene, mapdraw, limits = lims, clip = FALSE)
  hoverlayer = qlayer(scene, map_hover_draw, limits = lims, clip = FALSE,
    hoverMoveFun = map_hover, hoverLeaveFun = polygon_leave)

  qplotView(scene = scene)

}
