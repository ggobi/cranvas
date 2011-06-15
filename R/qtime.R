##' Draw a time plot
##'
##' @param data Mutaframe data to use
##' @param time The variable indicating time, which is displayed on
##' the horizontal axis
##' @param y The variable displayed on the vertical axis
##' @param period The variable to group the time series. Better to be
##' 'year','month', or other time resolutions. Default to be
##' null. When it is not null, the key U and D can be hit to separate
##' the groups or overlap them together to watch the patterns.
##' @param wrap The switch for wrapping or not when zooming in/out by
##' hitting right arrow or left arrow. Default to be TRUE.
##' @param shift Wrapping speed selector. The default possible speeds
##' are 1,7(for days a week),12(for months),24(for hours).
##' @param size Point size, default to be 2.
##' @param alpha Transparency level, 1=completely opaque, default to be 1.
##' @param asp Ratio between width and height of the plot.
##' @param main main title for the plot.
##' @param xlab label on horizontal axis, default is name of x variable
##' @param ylab label on vertical axis, default is name of y variable
##' @example cranvas/inst/examples/qtime-ex.R

qtime <- function(data, time, y, period=NULL, wrap=TRUE,
                  shift=c(1,7,12,24), size=2, alpha=1, asp=NULL, 
                  main=NULL, xlab=NULL, ylab=NULL,...){

#####################
  ## data processing ##----------
#####################

  arguments <- as.list(match.call()[-1])
  df <- data.frame(data)
  x <- eval(arguments$time, df)
  y <- as.data.frame(matrix(eval(arguments$y, df),nrow=nrow(df)),
                     stringsAsFactors=FALSE)
  pd <- eval(arguments$period, df)

  stopifnot(!is.null(x), !is.null(y))
  stopifnot(length(x) > 1, nrow(y) > 1,
            length(x) == nrow(y))

  ## variables/labels on the axes
  .levelX <- deparse(arguments$time)
  .levelY <- deparse(arguments$y)
  colnames(y) <- .levelY
  if (ncol(y)>1) {
    .levelY <- unlist(strsplit(substr(.levelY,3,nchar(.levelY)-1),','))
    .levelY <- gsub(" ","", .levelY)
  }
  if(is.null(xlab)) xlab <- .levelX
  if(is.null(ylab)) ylab <- paste(.levelY,collapse=', ')

  ## tdf: tmp data frame; zg: zoom group for wrapping
  tdf <- mutaframe(x=x,zg=rep(1,nrow(df)),pd=rep(1,nrow(df)),y)
  ## size for zoom in/out without wrapping
  zoomsize <- max(x,na.rm=TRUE)-min(x,na.rm=TRUE)

  ## restructure by time resolution
  if (!is.null(pd)){
    if (is.character(pd)) pd <- factor(pd)
    .period <- deparse(arguments$period)
    ## check whether period lengths are the same
    pdLen <- tapply(x,factor(pd),length)
    if (!all(pdLen==pdLen[1])) {
      warning('Period lengths are not the same.')
    }
    tdf <- mutaframe(x=rep(1:pdLen[1],length=length(x)),
                     zg=rep(1,nrow(df)),pd=pd, y)
    x <- rep(1:pdLen[1],length=length(x))
    zoomsize <- max(tdf$x,na.rm=TRUE)-min(tdf$x,na.rm=TRUE)
  }

  ## set plot range
  dataRanges <- c(extend_ranges(tdf$x),
                  extend_ranges(range(as.data.frame(tdf[,-(1:3)]))))
  lims <- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])
  #sy <- axis_loc(tdf$x)
  #sx <- axis_loc(unlist(data.frame(tdf[,-(1:3)])))

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
  .brange <- c(diff(dataRanges[c(1, 2)]), diff(dataRanges[c(3, 4)]))/30
  ## other
  hitscol <- 1
  vertconst <- 0

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

  ##  brush_mouse_release <- function(layer, event){
  ##    .bend <- as.numeric(event$pos())
  ##    idx <- tdf$x>min(.bstart[1],.bend[1]) &
  ##    y>min(.bstart[2],.bend[2]) &
  ##    tdf$x<max(.bstart[1],.bend[1]) &
  ##    y<max(.bstart[2],.bend[2])
  ##    selected(data) <- idx
  ##    if (length(idx)) qupdate(brush_layer)
  ##  }

  brush_mouse_move <- function(layer, event) {
    pos <- event$pos()
    .bpos <<- as.numeric(pos)
    ## simple click: don't change .brange
    if (!all(.bpos == .bstart) && (!.bmove)) {
      .brange <<- .bpos - .bstart
    }
    .new.brushed <- rep(FALSE, nrow(data))
    xrange <- .radius/root_layer$size$width() * diff(dataRanges[c(1, 2)])
    yrange <- .radius/root_layer$size$height() * diff(dataRanges[c(3, 4)])

    rect <- qrect(matrix(c(.bpos - .brange - c(xrange, yrange),
                           .bpos + c(xrange, yrange)),
                         2, byrow = TRUE))

    hits <- layer$locate(rect) + 1
    if (length(hits)<1) return()
    hitsrow <- round(hits %% nrow(data))
    hitsrow[hitsrow==0] <- nrow(data)
    hitscol <<- unique((hits-0.0000001)%/%nrow(data)+1)

    .new.brushed[hitsrow] <- TRUE
    data$.brushed <- mode_selection(data$.brushed, .new.brushed,
                                    mode = brush(data)$mode)
  }

  key_press <- function(layer, event){
    crt_range <- max(tdf$x)-min(tdf$x)+1

    if (event$key()==Qt$Qt$Key_Shift){
      shift <<- c(shift[-1],shift[1])
    }

    if (event$key()==Qt$Qt$Key_Right){
      ## arrow right

      if (wrap) {
        zoombound=crt_range-shift[1]
        if (shift[1]==1 & zoombound<3){
          zoombound <- 3
        } else if (shift[1]!=1 & zoombound<shift[1]){
          zoombound <- crt_range %% shift[1]
          if (!zoombound) zoombound <- shift[1]
        }
        tdf$x <- x%%zoombound
        tdf$zg <- ceiling(x/zoombound)
        if (sum(tdf$x==0)){
          tdf$zg[tdf$x==0] <- tdf$zg[which(tdf$x==0)-1]
          tdf$x[tdf$x==0] <- zoombound
        }
        dataRanges[1:2] <<- extend_ranges(tdf$x)
        lims <<- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])
      } else {
        zoomsize <<- zoomsize-2
        if (zoomsize < 2) zoomsize <<- 2
        tmpXzoom <- .bstart[1] + c(-0.5,0.5) * zoomsize
        tmpXzoom[1] <- max(tmpXzoom[1], min(x, na.rm=TRUE))
        tmpXzoom[2] <- min(tmpXzoom[2], max(x, na.rm=TRUE))
        dataRanges[1:2] <<- extend_ranges(tmpXzoom)
        lims <<- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])
        tdf$x <- x
        tdf$x[x<=tmpXzoom[1]]=NA
        tdf$x[x>=tmpXzoom[2]]=NA
      }
      qupdate(bg_layer)
      bg_layer$setLimits(lims)
      qupdate(xaxis_layer)
      xaxis_layer$setLimits(qrect(dataRanges[1:2], c(0, 1)))
      main_circle_layer$setLimits(lims)
      main_line_layer$setLimits(lims)
      brush_layer$setLimits(lims)
      query_layer$setLimits(lims)
    }

    if (event$key()==Qt$Qt$Key_Left){
      ## arrow left

      if (wrap) {
        zoombound <- crt_range+shift[1]
        if (zoombound>(zoomsize+1)) zoombound <- zoomsize+1
        tdf$x <- x%%zoombound
        tdf$zg <- ceiling(x/zoombound)
        if (sum(tdf$x==0)){
          tdf$zg[tdf$x==0] <- tdf$zg[which(tdf$x==0)-1]
          tdf$x[tdf$x==0] <- zoombound
        }
        dataRanges[1:2] <<- extend_ranges(tdf$x)
        lims <<- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])
      } else {
        zoomsize <<- zoomsize+2
        if (zoomsize > 2*(max(x)-min(x))) zoomsize <<- 2*(max(x)-min(x))
        tmpXzoom <- .bstart[1] + c(-0.5,0.5) * zoomsize
        tmpXzoom[1] <- max(tmpXzoom[1], min(x, na.rm=TRUE))
        tmpXzoom[2] <- min(tmpXzoom[2], max(x, na.rm=TRUE))
        dataRanges[1:2] <<- extend_ranges(tmpXzoom)
        lims <<- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])
        tdf$x <- x
        tdf$x[x<tmpXzoom[1]]=NA
        tdf$x[x>tmpXzoom[2]]=NA
      }
      qupdate(bg_layer)
      bg_layer$setLimits(lims)
      qupdate(xaxis_layer)
      xaxis_layer$setLimits(qrect(dataRanges[1:2], c(0, 1)))
      main_circle_layer$setLimits(lims)
      main_line_layer$setLimits(lims)
      brush_layer$setLimits(lims)
      query_layer$setLimits(lims)
    }

    if (!is.null(pd)) {
      if (event$key() == Qt$Qt$Key_U) {
        ## Key U (for Up)

        vertconst <<- vertconst + 0.05
        if (vertconst>1) vertconst <<- 1
        if (ncol(y)==1){
          tdf[,-(1:3)] <- unlist((y-min(y))/(max(y)-min(y))+
                                 (as.integer(pd)-1)*vertconst)
        } else {
          for (j in 1:ncol(y)) {
            tdf[,j+3] <- (y[,j]-min(y))/(max(y)-min(y))+
              (as.integer(pd)-1)*vertconst
          }
        }

        dataRanges[3:4] <<-  extend_ranges(range(data.frame(tdf[,-(1:3)])))
        lims <<- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])

        qupdate(bg_layer)
        bg_layer$setLimits(lims)
        qupdate(yaxis_layer)
        yaxis_layer$setLimits(qrect(c(0, 1), dataRanges[3:4]))
        main_circle_layer$setLimits(lims)
        main_line_layer$setLimits(lims)
        brush_layer$setLimits(lims)
        query_layer$setLimits(lims)
      }

      if (event$key() == Qt$Qt$Key_D) {
        ## Key D (for Down)

        vertconst <<- vertconst - 0.05
        if (vertconst<0) vertconst <<- 0
        if (!vertconst) {
          if (ncol(y)==1){
            tdf[,-(1:3)] <- unlist(y)
          } else {
            for (j in 1:ncol(y)) {
              tdf[,j+3] <- y[,j]
            }
          }
          dataRanges[3:4] <<-  extend_ranges(range(data.frame(tdf[,-(1:3)])))
          lims <<- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])
        } else {
          if (ncol(y)==1){
            tdf[,-(1:3)] <- unlist((y-min(y))/(max(y)-min(y))+
                                   (as.integer(pd)-1)*vertconst)
          } else {
            for (j in 1:ncol(y)) {
              tdf[,j+3] <- (y[,j]-min(y))/(max(y)-min(y))+
                (as.integer(pd)-1)*vertconst
            }
          }

          dataRanges[3:4] <<-  extend_ranges(range(data.frame(tdf[,-(1:3)])))
          lims <<- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])
        }

        qupdate(bg_layer)
        bg_layer$setLimits(lims)
        qupdate(yaxis_layer)
        yaxis_layer$setLimits(qrect(c(0, 1), dataRanges[3:4]))
        main_circle_layer$setLimits(lims)
        main_line_layer$setLimits(lims)
        brush_layer$setLimits(lims)
        query_layer$setLimits(lims)
      }
    }

    if (event$key() == Qt$Qt$Key_Up) {
      ## arrow up
      .radius <<- .radius + 1
    }

    if (event$key() == Qt$Qt$Key_Down & .radius > 0) {
      ## arrow down
      .radius <<- .radius - 1
    }

    if (event$key() == Qt$Qt$Key_Plus & .alpha < 1) {
      ## plus
      .alpha <<- 1.1 * .alpha
    }

    if (event$key() == Qt$Qt$Key_Minus & .alpha > 1/nrow(data)) {
      ## minus
      .alpha <<- 0.9 * .alpha
    }
    qupdate(main_circle_layer)
    qupdate(main_line_layer)
  }

  ## Display time information on hover (query) ----------------------
  .queryPos <- NULL

  query_draw <- function(item, painter, exposed, ...) {
    if (is.null(.queryPos)) return()

    xpos <- .queryPos[1]
    ypos <- .queryPos[2]

    xrange <- .radius/root_layer$size$width() * diff(dataRanges[c(1, 2)])
    yrange <- .radius/root_layer$size$height() * diff(dataRanges[c(3, 4)])

    rect <- qrect(matrix(c(xpos - xrange, ypos - yrange,
                           xpos + xrange, ypos + yrange),
                         2, byrow = TRUE))
    main_circle_layer$invalidateIndex()
    hits <- main_circle_layer$locate(rect) + 1

    ## Nothing under mouse?
    if (length(hits) <1 ) return()
    hitsrow <- round(hits %% nrow(data))
    hitsrow[hitsrow==0] <- nrow(data)
    hitscol <- (hits-0.0000001)%/%nrow(data)+1
    if (is.null(pd)) {hitspd <- NULL} else {hitspd <- .period}
    info <- as.data.frame(data[hitsrow, c(.levelX, .levelY[hitscol],hitspd)])
    ## print(info)
    ## browser()

    ## Work out label text
    idx <- names(info)
    if (length(hits) == 1) {
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
    hflag <- dataRanges[2] - xpos > bgwidth
    vflag <- ypos - dataRanges[3] > bgheight
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


  xaxis <- function(layer, painter) {
    sx <- axis_loc(dataRanges[1:2])
    draw_x_axes_with_labels_fun(painter, c(dataRanges[1:2],1,5),
                                axisLabel =sx, labelHoriPos = sx,
                                name = xlab)
  }

  yaxis <- function(layer, painter) {
    if (is.null(pd) | !vertconst){
        sy <- axis_loc(dataRanges[3:4])
    } else {
        sy <- (as.integer(unique(pd))-1)*vertconst
    }
    if (!vertconst) {aL <- sy} else {aL <- unique(pd)}
    draw_y_axes_with_labels_fun(painter, c(1,5, dataRanges[3:4]),
                                axisLabel = aL, labelVertPos = sy,
                                name = ifelse(!vertconst,ylab,.period))
  }

  grid <- function(layer, painter) {
    sx <- axis_loc(dataRanges[1:2])
    sy <- axis_loc(dataRanges[3:4])
    draw_grid_with_positions_fun(painter, dataRanges, sx, sy)
  }

  main_circle_draw <- function(layer,painter){
    for (j in 1:ncol(y)) {
      color=gray(seq(0,0.6,length=max(tdf$zg)))
      for (k in unique(tdf$pd)) {
        for (i in 1:max(tdf$zg)) {
          if (sum(tdf$zg==i & tdf$pd==k)){
            qdrawCircle(painter,tdf[tdf$zg==i & tdf$pd==k,1],
                        tdf[tdf$zg==i & tdf$pd==k,j+3],
                        r=.radius,
                        fill=alpha(color[max(tdf$zg)+1-i],.alpha),
                        stroke=alpha(color[max(tdf$zg)+1-i],.alpha))
          }
        }
      }
    }
  }

  main_line_draw <- function(layer,painter){
    for (j in 1:ncol(y)) {
      color=gray(seq(0,0.6,length=max(tdf$zg)))
      for (k in unique(tdf$pd)) {
        for (i in 1:max(tdf$zg)) {
          if (sum(tdf$zg==i & tdf$pd==k)){
            qdrawLine(painter,tdf[tdf$zg==i & tdf$pd==k,1],
                      tdf[tdf$zg==i & tdf$pd==k,j+3],
                      stroke=alpha(color[max(tdf$zg)+1-i],.alpha))
          }
        }
      }
    }
  }

  brush_draw <- function(layer, painter) {
    .brushed = data$.brushed
    if (.brush) {
      if (!any(is.na(.bpos))) {
        qlineWidth(painter) = brush(data)$size
        qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - .brange[2],
                  .bpos[1], .bpos[2], stroke = brush(data)$color)
      }

      hdata <- subset(data.frame(tdf$x,y=tdf[,hitscol+3]), .brushed)

      if (nrow(hdata) > 0) {
        ## draw the brush rectangle
        if (!any(is.na(.bpos))) {
          qlineWidth(painter) <- brush(data)$size
          qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - .brange[2],
                    .bpos[1], .bpos[2], stroke = brush(data)$color)
        }
        ## (re)draw brushed data points
        brushx <- hdata$tdf.x
        brushy <- hdata[,-1]
        fill = brush(data)$color
        stroke = brush(data)$color
        radius <- .radius

        if (is.vector(brushy)){
          qdrawCircle(painter, x = brushx, y = brushy,
                      r = radius, fill = fill, stroke = stroke)
        } else {
          for (i in 1:ncol(brushy)){
            qdrawCircle(painter, x = brushx, y = brushy[,i],
                        r = radius, fill = fill, stroke = stroke)
          }
        }
      }
    }

  }


#####################
  ## draw the canvas ##----------
#####################

  xWidth <- 800
  yWidth <- 500
  if (!is.null(asp)) xWidth <- round(yWidth*asp)

  scene <- qscene()
  root_layer <- qlayer(scene)
  ##root_layer$setGeometry(qrect(0, 0, xWidth, yWidth))
  xaxis_layer <- qlayer(parent=root_layer, paintFun = xaxis,
                        limits = qrect(dataRanges[1:2], c(0, 1)),
                        row=2, col=1, clip=FALSE)
  yaxis_layer <- qlayer(parent=root_layer, paintFun = yaxis,
                        limits = qrect(c(0, 1), dataRanges[3:4]),
                        row=1, col=0, clip=FALSE)
  bg_layer <- qlayer(parent= root_layer, paintFun = grid,
                     limits = lims, row=1, col=1, clip=FALSE)
  main_circle_layer <- qlayer(root_layer,paintFun=main_circle_draw,
                              mousePressFun=brush_mouse_press,
                              mouseReleaseFun=brush_mouse_move,
                              mouseMove = brush_mouse_move,
                              keyPressFun=key_press,
                              limits=lims, row = 1, col = 1, clip=FALSE)
  main_line_layer <- qlayer(root_layer,paintFun=main_line_draw,
                            limits=lims, row = 1, col = 1, clip=FALSE)
  brush_layer <- qlayer(root_layer, brush_draw,
                        limits=lims, row = 1, col = 1, clip=FALSE)
  query_layer <- qlayer(root_layer, query_draw,
                        limits = lims, hoverMoveFun = query_hover,
                        hoverLeaveFun = query_hover_leave,
                        row = 1, col = 1, clip=FALSE)

  layout = root_layer$gridLayout()
  layout$setRowPreferredHeight(0, 10)
  layout$setColumnPreferredWidth(0, 60)
  layout$setRowPreferredHeight(2, 60)
  layout$setColumnPreferredWidth(2, 10)
  layout$setColumnMaximumWidth(2, 15)
  layout$setRowStretchFactor(0, 0)
  layout$setColumnStretchFactor(0, 0)
  layout$setRowStretchFactor(2, 0)

  view <- qplotView(scene=scene)
  if (!is.null(main)) view$setWindowTitle(main)

######################
  ## add some listeners #
######################
  ## if (is.mutaframe(data)) {
  func <- function(i, j) {
    switch(j, .brushed = qupdate(brush_layer),
           .color = qupdate(main_circle_layer),
           { ## any other event
             main_circle_layer$invalidateIndex()
             qupdate(main_circle_layer)
             qupdate(brush_layer)
           })
  }
  add_listener(data, func)
  ## }

  view
}
