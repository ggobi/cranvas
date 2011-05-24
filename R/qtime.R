qtime <- function(time,y,data,size=2,alpha=1,aspect.ratio=NULL){
  
#####################
## data processing ##----------
#####################
  
  arguments <- as.list(match.call()[-1])
  df <- data.frame(data)
  x <- eval(arguments$time, df)
  y <- eval(arguments$y, df)
  tdf <- mutaframe(x=x,zg=rep(1,nrow(df))) # tdf: tmp data frame; zg: zoom group.
  .levelX <- deparse(arguments$time)
  .levelY <- deparse(arguments$y)

  dataRanges <- c(range(tdf$x), range(y))
  windowRanges <- dataRanges
  lims <- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])
#  sy <- get_axisPos(tdf$x)
#  sx <- get_axisPos(y)
  sy <- .axis.loc(tdf$x)
  sx <- .axis.loc(y)

  ## parameters for datalayer
  .radius <- size
  .alpha <- alpha
  ## parameters event handling
  .startBrush <- NULL
  .endBrush <- NULL
  ## whether in the brush mode
  .brush <- TRUE
  ## mouse position
  .bpos <- c(NA, NA)
  ## drag start
  .bstart <- c(NA, NA)
  ## move brush?
  .bmove <- TRUE
  ## brush range: horizontal and vertical
  .brange <- c(diff(windowRanges[c(1, 2)]), diff(windowRanges[c(3, 4)]))/30

  
####################
## event handlers ##----------
####################
  
  brush_mouse_press <- function(layer, event) {
    .bstart <<- as.numeric(event$pos())
    if (event$button() == Qt$Qt$RightButton) {
      .bmove <<- FALSE
    }
    if (event$button() == Qt$Qt$LeftButton) {
      .bmove <<- TRUE
    }
  }

  brush_mouse_release <- function(layer, event){
    .bend <- as.numeric(event$pos())
    idx <- tdf$x>min(.bstart[1],.bend[1]) &
    y>min(.bstart[2],.bend[2]) &
    tdf$x<max(.bstart[1],.bend[1]) &
    y<max(.bstart[2],.bend[2])
    selected(data) <- idx
    if (length(idx)) qupdate(brush_layer)
  }

  brush_mouse_move <- function(layer, event) {
    pos <- event$pos()
    .bpos <<- as.numeric(pos)
    ## simple click: don't change .brange
    if (!all(.bpos == .bstart) && (!.bmove)) {
      .brange <<- .bpos - .bstart
    }
    .new.brushed <- rep(FALSE, nrow(data))
    xrange <- .radius/root_layer$size$width() * diff(windowRanges[c(1, 2)])
    yrange <- .radius/root_layer$size$height() * diff(windowRanges[c(3, 4)])

    rect <- qrect(matrix(c(.bpos - .brange - c(xrange, yrange),
                           .bpos + .brange + c(xrange, yrange)),
                         2, byrow = TRUE))

    hits <- layer$locate(rect)[-1] + 1
    if (length(hits)<=1)return()
    print(hits)
        
    .new.brushed[hits] <- TRUE
    data$.brushed <- mode_selection(data$.brushed, .new.brushed,
                                    mode = brush(data)$mode)
  }

  key_press <- function(layer, event){
    crt_range <- max(tdf$x)-min(tdf$x)+1

    if (event$key()==Qt$Qt$Key_Plus){
      ## plus
      zoombound=min(round(0.96*crt_range),crt_range-1)
      if (zoombound<3)zoombound <- 3
      tdf$x <- x%%zoombound
      tdf$zg <- ceiling(x/zoombound)
      if (sum(tdf$x==0)){
        tdf$zg[tdf$x==0] <- tdf$zg[which(tdf$x==0)-1]
        tdf$x[tdf$x==0] <- zoombound
      }
      dataRanges <<- c(range(tdf$x), range(y))
      windowRanges <<- dataRanges
      sy <<- .axis.loc(tdf$x)
      sx <<- .axis.loc(dataRanges[3:4])
      lims <<- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])
      qupdate(bg_layer)
      main_layer$setLimits(lims)
      brush_layer$setLimits(lims)
    }

    if (event$key()==Qt$Qt$Key_Minus){
      ## minus
      zoombound=max(round(crt_range*25/24),crt_range+1)
      if (zoombound>(max(x)-min(x)+1)) zoombound <- max(x)-min(x)+1
      tdf$x <- x%%zoombound
      tdf$zg <- ceiling(x/zoombound)
      if (sum(tdf$x==0)){
        tdf$zg[tdf$x==0] <- tdf$zg[which(tdf$x==0)-1]
        tdf$x[tdf$x==0] <- zoombound
      }
      dataRanges <<- c(range(tdf$x), range(y))
      windowRanges <<- dataRanges
      sy <<- .axis.loc(tdf$x)
      sx <<- .axis.loc(dataRanges[3:4])
      lims <<- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])
      qupdate(bg_layer)
      main_layer$setLimits(lims)
      brush_layer$setLimits(lims)
    }

    if (event$key() == Qt$Qt$Key_Up) {
      ## arrow up
      .radius <<- .radius + 1
    }

    if (event$key() == Qt$Qt$Key_Down & .radius > 0) {
      ## arrow down
      .radius <<- .radius - 1
    }

    if (event$key() == Qt$Qt$Key_Right & .alpha < 1) {
      ## arrow right
      .alpha <<- 1.1 * .alpha
      main_layer$setOpacity(.alpha)
    }

    if (event$key() == Qt$Qt$Key_Left & .alpha > 1/nrow(data)) {
      ## arrow left
      .alpha <<- 0.9 * .alpha
      main_layer$setOpacity(.alpha)
    }
    qupdate(main_layer)
    ## qupdate(xaxis)
  }

  ## Display time information on hover (query) ----------------------
  .queryPos <- NULL
    
  query_draw <- function(item, painter, exposed, ...) {
    if (is.null(.queryPos)) return()
        
    xpos <- .queryPos[1]
    ypos <- .queryPos[2]
        
    xrange <- .radius/root_layer$size$width() * diff(windowRanges[c(1, 2)])
    yrange <- .radius/root_layer$size$height() * diff(windowRanges[c(3, 4)])
    
    rect <- qrect(matrix(c(xpos - xrange, ypos - yrange,
                           xpos + xrange, ypos + yrange),
                         2, byrow = TRUE))
    hits <- main_layer$locate(rect) + 1

    ## Nothing under mouse?
    if (length(hits) <=1 ) return()
    
    info <- as.data.frame(data[as.integer(hits[2]), c(.levelX, .levelY)])
    ## print(info)
    ## browser()
        
    ## Work out label text
    idx <- names(info)
    if (length(hits) == 2) {
      infodata <- as.character(unlist(info[, idx]))
      infostring <- paste(idx, infodata, collapse = "\n", sep = ": ")
    }
    else {
      xymin <- unlist(lapply(info[, idx], min, na.rm = T))
      xymax <- unlist(lapply(info[, idx], max, na.rm = T))
      infostring <- paste(idx, paste(xymin, xymax, sep = " - "),
                          collapse = "\n", sep = ": ")
      infostring <- paste(" xxhitsxx points\n", infostring)
      infostring <- gsub("xxhitsxx", length(hits), infostring)
    }
    bgwidth <- qstrWidth(painter, infostring)
    bgheight <- qstrHeight(painter, infostring)
        
    ## adjust drawing directions when close to the boundary
    hflag <- windowRanges[2] - xpos > bgwidth
    vflag <- ypos - windowRanges[3] > bgheight
    qdrawRect(painter, xpos, ypos,
              xpos + ifelse(hflag, 1, -1) * bgwidth,
              ypos + ifelse(vflag, -1, 1) * bgheight,
              stroke = rgb(1, 1, 1),
              fill = rgb(1, 1, 1, 0.9))
        
    qstrokeColor(painter) <- brush(data)$label.color
    qdrawText(painter, infostring, xpos, ypos,
              halign = ifelse(hflag, "left", "right"),
              valign = ifelse(vflag, "top", "bottom"))
        
  }
    
  query_hover <- function(item, event, ...) {
    .queryPos <<- as.numeric(event$pos())
    qupdate(query_layer)
  }
    
  query_hover_leave <- function(item, event, ...) {
    .queryPos <<- NULL
    qupdate(query_layer)
  }
    
  
  
############
## layers ##----------
############
  
  bg_draw <- function(item, painter, exposed) {
    draw_grid_with_positions_fun(painter, dataRanges, sy, sx)

    draw_x_axes_with_labels_fun(painter, dataRanges,
                                axisLabel = sy, labelHoriPos = sy,
                                name = NULL)

    draw_y_axes_with_labels_fun(painter, dataRanges,
                                axisLabel = sx, labelVertPos = sx,
                                name = NULL)   
  }
    
  main_draw <- function(layer,painter){
    color=gray(seq(0,0.6,length=max(tdf$zg)))
    for (i in 1:max(tdf$zg)) {
      qdrawCircle(painter,tdf[tdf$zg==i,1],
                  y[tdf$zg==i],r=.radius,
                  fill=color[i],stroke=color[i])
      qdrawLine(painter,tdf[tdf$zg==i,1],
                y[tdf$zg==i],stroke=color[i])
    }
  }
  
  brush_draw <- function(layer, painter) {
    ## idx <- selected(data)
    ## if (any(idx)){
    ##   txt <- paste("Point ",rownames(df)[idx],"\nx: ",
    ##                x[idx],"\nValue: ",y[idx],
    ##                sep="")
    ##   qdrawText(painter,txt,tdf[idx,1],y[idx])
    ## }
    .brushed = data$.brushed
    if (.brush) {
      if (!any(is.na(.bpos))) {
        qlineWidth(painter) = brush(data)$size
        qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - .brange[2], 
                  .bpos[1], .bpos[2], stroke = brush(data)$color)
      }
            
      hdata <- subset(data.frame(tdf$x,y), .brushed)
      #print(hdata)
            
      if (nrow(hdata) > 0) {
        ## draw the brush rectangle
        if (!any(is.na(.bpos))) {
          qlineWidth(painter) <- brush(data)$size
          qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - .brange[2], 
                    .bpos[1], .bpos[2], stroke = brush(data)$color)
        }
        ## (re)draw brushed data points
        brushx <- hdata$x
        brushy <- hdata$y
        fill = brush(data)$color
        stroke = brush(data)$color
        radius <- .radius
                
        qdrawCircle(painter, x = brushx, y = brushy,
                    r = radius, fill = fill, stroke = stroke)
      }
    }

  }

  
#####################
## draw the canvas ##----------
#####################

  xWidth <- 800
  yWidth <- 500
  if (!is.null(aspect.ratio)) xWidth <- round(yWidth*aspect.ratio)
  
  scene <- qscene()
  root_layer <- qlayer(scene)
  root_layer$setGeometry(qrect(0, 0, xWidth, yWidth))
  ## xaxis <- qaxis(root_layer, data = tdf$x, side = 1,
  ##                limits=range(tdf$x),
  ##                row = 2, col = 1)
  ## yaxis <- qaxis(root_layer, data = y, side = 2,
  ##                limits=range(y),
  ##                row = 1, col = 0)
  bg_layer <- qlayer(parent = root_layer, paintFun = bg_draw,
                     limits=lims, row = 1, col = 1)
  main_layer <- qlayer(root_layer,paintFun=main_draw,
                       mousePressFun=brush_mouse_press,
                       mouseReleaseFun=brush_mouse_release,
                       mouseMove = brush_mouse_move, 
                       keyPressFun=key_press,
                       limits=lims, row = 1, col = 1)
  brush_layer <- qlayer(root_layer, brush_draw,
                        limits=qrect(range(tdf$x),
                          range(y)), row = 1, col = 1)
  query_layer <- qlayer(root_layer, query_draw,
                        limits = lims, hoverMoveFun = query_hover,
                        hoverLeaveFun = query_hover_leave,
                        row = 1, col = 1)

  layout = root_layer$gridLayout()
  layout$setRowPreferredHeight(0, 10)
  layout$setColumnPreferredWidth(0, 70)
  layout$setRowPreferredHeight(2, 30)
  layout$setColumnPreferredWidth(2, 10)
  layout$setColumnMaximumWidth(2, 15)
  layout$setRowStretchFactor(0, 0)
  layout$setColumnStretchFactor(0, 0)
  layout$setRowStretchFactor(2, 0)

  view <- qplotView(scene=scene)
  view
}
