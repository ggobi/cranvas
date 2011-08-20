##' Draw a time plot
##'
##' Draw a time-series plot.
##'
##' Arrow up/down: in-/de-crease size of points.
##' Arrow left/right: wrap the time series when wrap=TRUE, while zoom
##' in/out with the center of the last clicked dot when wrap=FALSE.
##' Shift + right: when wrap=TRUE, the time series will be folded
##' directly to the width of maximal value in argument shift.
##' Shift + left: time series will be backed to the original xaxis
##' position, no matter wrap is TRUE or FALSE.
##' Key '+'/'-': de-/in-crease alpha level (starts at alpha=1 by
##' default).
##' Key 'u'/'d': separate/mix the series groups by shifting them up
##' and down.
##' Shift + 'u'/'d': for multivariate y's, separate/mix them by shifting
##' up and down.
##' Key 'g': change the wrapping speed circularly in the values of
##' parameter 'shift'.
##' @param time The variable indicating time, which is displayed on
##' the horizontal axis
##' @param y The variable(s) displayed on the vertical axis. It must 
##' be a formula with only right hand side at the moment. See examples.
##' @param data Mutaframe data to use
##' @param period The variable to group the time series. Better to be
##' 'year','month', or other time resolutions. Default to be
##' null. When it is not null, the key U and D can be hit to separate
##' the groups or overlap them together to watch the patterns.
##' @param group Similar to period, but is used for longitudinal data grouping.
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
##' @example inst/examples/qtime-ex.R
##' @export


qtime <- function(time, y, data, period=NULL, group=NULL, wrap=TRUE,
                  shift=c(1,7,12,24), size=2, alpha=1, asp=NULL, 
                  main=NULL, xlab=NULL, ylab=NULL,...){

#####################
  ## data processing ##----------
#####################

  call <- as.list(match.call()[-1])
  b <- brush(data)
  meta <- Time.meta$new(varname = list(x = as.character(call$time)))
 
  ## X axis setting
  meta$time <- eval(call$time, as.data.frame(data))
  meta$xtmp <- meta$time
  meta$xlab <- ifelse(is.null(xlab), meta$varname$x, xlab)
  
  ## Y axis setting
  if (inherits(y, 'formula')){
    if (length(y)!=2){stop("Wrong formula format.")}
    meta$varname$y <- all.vars(y)
  } else {
    meta$varname$y <- as.character(call$y) 
  }
  meta$yorig <- as.data.frame(data[,meta$varname$y,drop=FALSE])
  meta$y <- meta$yorig
  if (ncol(meta$yorig)>1) {
    for (i in 1:ncol(meta$yorig)) {
      meta$y[,i] <- (meta$yorig[,i] - min(meta$yorig[,i], na.rm = TRUE))/
        diff(range(meta$yorig[,i], na.rm = TRUE))
    }
  }
  meta$ytmp <- meta$y
  meta$ylab <- ifelse(is.null(ylab), paste(meta$varname$y,collapse=', '), ylab)
 
  ## Period for time series / Group for panel data
  if (!is.null(call$period)) {
    meta$varname$g <- as.character(call$period)
    meta$group <- as.factor(data[,meta$varname$g])
    pdLen <- tapply(meta$time,meta$group,length)
    if (!all(pdLen==pdLen[1])) {
      warning('Period lengths are not the same.')
    }
    ## need to be modified here !!
    meta$time <- rep(1:pdLen[1],length=length(meta$time)) 
    meta$xtmp <- meta$time
    meta$vargroup <- meta$group
  } else  if (!is.null(call$group)) {
    meta$varname$g <- as.character(call$group)
    meta$group <- as.factor(data[,meta$varname$g])
    meta$vargroup <- meta$group
  } else {
    meta$vargroup <- rep(1, nrow(data))
  }

  ## Other settings
  meta$wrap.group <- rep(1, nrow(data))
  meta$wrap.shift <- shift
  meta$hitscol <- 1
  meta$hitsrow <- NULL
  meta$vertconst <- 0

  ## Range etc.
  meta$zoomsize <- diff(range(meta$xtmp, na.rm = TRUE))
  meta$datarange <- c(extend_ranges(meta$xtmp),
                      extend_ranges(range(meta$ytmp, na.rm = TRUE)))
  meta$lims <- qrect(meta$datarange[c(1, 2)],
                     meta$datarange[c(3, 4)])

  ## Radius etc.
  meta$radius <- size
  meta$alpha <- alpha
  meta$stroke <- data$.color
  meta$fill <- data$.fill

  ## Brush etc.
  meta$pos <- c(NA, NA)
  meta$query.pos <- NULL
  meta$start <- c(NA, NA)
  meta$brush.move <- TRUE
  meta$brush.size <- c(diff(meta$datarange[c(1, 2)]),
                       diff(meta$datarange[c(3, 4)]))/30

  ## Title
  if (is.null(main)) {
    main = paste("Time Plot of", meta$varname$x, "And", 
      paste(meta$varname$y,collapse=', '))
  }

  ## SetLimits Function
  setLimitsFunc <- function(x){
    qupdate(bg_layer)
    bg_layer$setLimits(meta$lims)
  if (x=="x") {
      qupdate(xaxis_layer)
      xaxis_layer$setLimits(qrect(meta$datarange[1:2], c(0, 1)))
	} else if (x=="y") {
      qupdate(yaxis_layer)
      yaxis_layer$setLimits(qrect(c(0, 1), meta$datarange[3:4]))
    }
	main_circle_layer$setLimits(meta$lims)
    main_line_layer$setLimits(meta$lims)
    brush_layer$setLimits(meta$lims)
    query_layer$setLimits(meta$lims)
  }
  
####################
  ## event handlers ##----------
####################

  brush_mouse_press <- function(layer, event) {
    meta$start <- as.numeric(event$pos())
    if (event$button() == Qt$Qt$RightButton) {
      meta$brush.move <- FALSE
      b$cursor <- 2L
    }
    if (event$button() == Qt$Qt$LeftButton) {
      meta$brush.move <- TRUE
      b$cursor <- 0L
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
    .new.brushed <- rep(FALSE, nrow(data))
    rect <- qrect(update_brush_size(meta))
    hits <- layer$locate(rect) + 1
    if (length(hits)<1) {
      selected(data) <- FALSE
      return()
    }
    meta$hitsrow <- round(hits %% nrow(data))
    meta$hitsrow[meta$hitsrow==0] <- nrow(data)
    meta$hitscol <- (hits-0.0000001)%/%nrow(data)+1
    
    .new.brushed[meta$hitsrow] <- TRUE
    selected(data) <- mode_selection(selected(data), .new.brushed,
                                     mode = b$mode)
    if (!is.null(meta$group)) self_link(data)
  }

  key_press <- function(layer, event){
    crt_range <- diff(range(meta$xtmp,na.rm=TRUE))+1

    if (event$key()==Qt$Qt$Key_G){
      ## key G for gear(shift the wrapping speed)
      
      meta$wrap.shift <- c(meta$wrap.shift[-1],meta$wrap.shift[1])
    } else if (event$key()==Qt$Qt$Key_Right){
      ## arrow right

      if (wrap) {
        if (is.null(call$period) & is.null(call$group) & event$modifiers() == Qt$Qt$ShiftModifier) {
          zoombound <- max(meta$wrap.shift)
          if (zoombound<2) zoombound <- diff(range(meta$x,na.rm=TRUE))/4
          meta$xtmp <- meta$time %% zoombound
          meta$wrap.group <- ceiling(meta$time/zoombound)
          if (sum(meta$xtmp==0)){
            meta$wrap.group[meta$xtmp==0] <- meta$wrap.group[which(meta$xtmp==0)-1]
            meta$xtmp[meta$xtmp==0] <- zoombound
          }
          meta$datarange[1:2] <- extend_ranges(meta$xtmp)
          meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])
        } else {
        zoombound <- crt_range-meta$wrap.shift[1]
        if (meta$wrap.shift[1]==1 & zoombound<3){
          zoombound <- 3
        } else if (meta$wrap.shift[1]!=1 & zoombound<meta$wrap.shift[1]){
          zoombound <- crt_range %% meta$wrap.shift[1]
          if (!zoombound) zoombound <- meta$wrap.shift[1]
        }
        meta$xtmp <- meta$time %% zoombound
        meta$wrap.group <- ceiling(meta$time/zoombound)
        if (sum(meta$xtmp==0)){
          meta$wrap.group[meta$xtmp==0] <- meta$wrap.group[which(meta$xtmp==0)-1]
          meta$xtmp[meta$xtmp==0] <- zoombound
        }
        meta$datarange[1:2] <- extend_ranges(meta$xtmp)
        meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])
        }
      } else {
        meta$zoomsize <- meta$zoomsize-2
        if (meta$zoomsize < 2) meta$zoomsize <- 2
        tmpXzoom <- meta$start[1] + c(-0.5,0.5) * meta$zoomsize
        tmpXzoom[1] <- max(tmpXzoom[1], min(meta$time, na.rm=TRUE))
        tmpXzoom[2] <- min(tmpXzoom[2], max(meta$time, na.rm=TRUE))
        meta$datarange[1:2] <- extend_ranges(tmpXzoom)
        meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])
        meta$xtmp <- meta$time
        meta$xtmp[meta$time<=tmpXzoom[1]]=NA
        meta$xtmp[meta$time>=tmpXzoom[2]]=NA
      }
      setLimitsFunc("x")
    } else if (event$key()==Qt$Qt$Key_Left){
      ## arrow left
      
      if (event$modifiers() == Qt$Qt$ShiftModifier) {
          meta$xtmp <- meta$time
          meta$wrap.group <- 1
          meta$zoomsize <- diff(range(meta$xtmp, na.rm = TRUE))
          meta$datarange[1:2] <- extend_ranges(meta$xtmp)
          meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])
      } else {
      if (wrap) {
        zoombound <- crt_range+meta$wrap.shift[1]
        if (zoombound>(meta$zoomsize+min(meta$time,na.rm=TRUE))) {
          zoombound <- meta$zoomsize+min(meta$time,na.rm=TRUE)
        }
        meta$xtmp <- meta$time %% zoombound
        meta$wrap.group <- ceiling(meta$time/zoombound)
        if (sum(meta$xtmp==0)){
          meta$wrap.group[meta$xtmp==0] <- meta$wrap.group[which(meta$xtmp==0)-1]
          meta$xtmp[meta$xtmp==0] <- zoombound
        }
        while (diff(range(meta$xtmp,na.rm=TRUE))+1 <= crt_range &
               zoombound<meta$zoomsize+min(meta$time,na.rm=TRUE)) {
          zoombound <- zoombound+meta$wrap.shift[1]
          if (zoombound>(meta$zoomsize+min(meta$time,na.rm=TRUE))) {
            zoombound <- meta$zoomsize+min(meta$time,na.rm=TRUE)
          }
          meta$xtmp <- meta$time %% zoombound
          meta$wrap.group <- ceiling(meta$time/zoombound)
          if (sum(meta$xtmp==0)){
            meta$wrap.group[meta$xtmp==0] <- meta$wrap.group[which(meta$xtmp==0)-1]
            meta$xtmp[meta$xtmp==0] <- zoombound
          }
        }
        meta$datarange[1:2] <- extend_ranges(meta$xtmp)
        meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])        
      } else {
        meta$zoomsize <- meta$zoomsize+2
        if (meta$zoomsize > 2*diff(range(meta$time,na.rm=TRUE))) {
          meta$zoomsize <- 2*diff(range(meta$time,na.rm=TRUE))
        }
        tmpXzoom <- meta$start[1] + c(-0.5,0.5) * meta$zoomsize
        tmpXzoom[1] <- max(tmpXzoom[1], min(meta$time, na.rm=TRUE))
        tmpXzoom[2] <- min(tmpXzoom[2], max(meta$time, na.rm=TRUE))
        meta$datarange[1:2] <- extend_ranges(tmpXzoom)
        meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])
        meta$xtmp <- meta$time
        meta$xtmp[meta$time<tmpXzoom[1]]=NA
        meta$xtmp[meta$time>tmpXzoom[2]]=NA
      }}
	  setLimitsFunc("x")

    } else if (event$key() == Qt$Qt$Key_U) {
        ## Key U (for Up)
        
        if (ncol(meta$y)>1 & event$modifiers() == Qt$Qt$ShiftModifier) {
          for (i in 1:ncol(meta$y)){
            meta$ytmp[,i] <- meta$y[,i]+i
          }
          meta$datarange[3:4] <-  extend_ranges(range(meta$ytmp,na.rm=TRUE))
          meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])
		  setLimitsFunc("y")
        } else if (!is.null(meta$group) & length(meta$group)>0) {
          meta$vertconst <- meta$vertconst + 0.05
          if (meta$vertconst>1) meta$vertconst <- 1
          if (ncol(meta$y)==1){
            meta$ytmp <- (meta$y-min(meta$y,na.rm=TRUE))/
                         diff(range(meta$y,na.rm=TRUE))+
                         (as.integer(meta$group)-1)*meta$vertconst
          } else {
            for (j in 1:ncol(meta$y)) {
              meta$ytmp[,j] <- (meta$y[,j]-min(meta$y,na.rm=TRUE))/
                diff(range(meta$y,na.rm=TRUE))+
                  (as.integer(meta$group)-1)*meta$vertconst
            }
          }
          meta$datarange[3:4] <-  extend_ranges(range(meta$ytmp,na.rm=TRUE))
          meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])
          setLimitsFunc("y")
        }
    } else if (event$key() == Qt$Qt$Key_D) {
        ## Key D (for Down)
        
        if (ncol(meta$y)>1 & event$modifiers() == Qt$Qt$ShiftModifier) {
          meta$ytmp <- meta$y
          meta$datarange[3:4] <-  extend_ranges(range(meta$ytmp,na.rm=TRUE))
          meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])      
          setLimitsFunc("y")
        } else if (!is.null(meta$group) & length(meta$group)>0) {
          meta$vertconst <- meta$vertconst - 0.05
          if (meta$vertconst<0) meta$vertconst <- 0
          if (!meta$vertconst) {
            meta$ytmp <- meta$y
            meta$datarange[3:4] <-  extend_ranges(range(meta$ytmp,na.rm=TRUE))
            meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])         
          } else {
            if (ncol(meta$y)==1){
             meta$ytmp <- (meta$y-min(meta$y,na.rm=TRUE))/
                           diff(range(meta$y,na.rm=TRUE))+
                           (as.integer(meta$group)-1)*meta$vertconst
            } else {
              for (j in 1:ncol(meta$y)) {
                meta$ytmp[,j] <- (meta$y[,j]-min(meta$y,na.rm=TRUE))/
                  diff(range(meta$y,na.rm=TRUE))+
                  (as.integer(meta$group)-1)*meta$vertconst
              }
            }
          }
          meta$datarange[3:4] <-  extend_ranges(range(meta$ytmp,na.rm=TRUE))
          meta$lims <- qrect(meta$datarange[c(1, 2)], meta$datarange[c(3, 4)])       
          setLimitsFunc("y")
        } 
    }
    if (length(i <- which(event$key() == c(Qt$Qt$Key_Up, Qt$Qt$Key_Down)))) {
      ## arrow up/down
      meta$radius <- max(0.1, meta$radius + c(1, -1)[i])
    } else if (length(i <- which(event$key() == c(Qt$Qt$Key_Plus, Qt$Qt$Key_Minus)))) {
      ## arrow plus/minus: alpha blending
      meta$alpha <- max(0.01, 1/nrow(data), min(1, c(1.1, 0.9)[i] * meta$alpha))
    }
    qupdate(main_circle_layer)
    qupdate(main_line_layer)
  }

  ## Display time information on hover (query) ----------------------
  query_draw <- function(item, painter, exposed, ...) {
    if (is.null(meta$query.pos)) return()

    xpos <- meta$query.pos[1]
    ypos <- meta$query.pos[2]
    
    queryaround <- ifelse(meta$radius<=4,8/meta$radius,1)
    xrange <- meta$radius/root_layer$size$width() *
      diff(meta$datarange[c(1, 2)]) * queryaround
    yrange <- meta$radius/root_layer$size$height() *
      diff(meta$datarange[c(3, 4)]) * queryaround

    rect <- qrect(matrix(c(xpos - xrange, ypos - yrange,
                           xpos + xrange, ypos + yrange),
                         2, byrow = TRUE))
    main_circle_layer$invalidateIndex()
    hits <- main_circle_layer$locate(rect) + 1

    ## Nothing under mouse?
    if (length(hits) < 1) return()

    hitsrow <- round(hits %% nrow(data))
    hitsrow[hitsrow==0] <- nrow(data)
    hitscol <- (hits-0.0000001)%/%nrow(data)+1

    if (length(hits) > 1) {
      hitsdist <- rep(0,length(hits))
      for (i in 1:length(hits)){
        hitsdist[i] <- sqrt((xpos-meta$xtmp[hitsrow[i]])^2 +
                            (ypos-meta$ytmp[hitsrow[i],hitscol[i]])^2)
      }
      distidx <- which(hitsdist==min(hitsdist,na.rm=TRUE))
      hits <- hits[distidx]
      hitsrow <- hitsrow[distidx]
      hitscol <- hitscol[distidx]
    }
    if (is.null(meta$group)) {hitspd <- NULL} else {hitspd <- meta$varname$g}
    info <- as.data.frame(data[hitsrow,
                               c(meta$varname$x, meta$varname$y[hitscol],hitspd)])

    ## label position
    labelxpos <- mean(meta$xtmp[hitsrow])
    tmp <- hits
    for (i in 1:length(hits)){tmp[i]=meta$ytmp[hitsrow[i],hitscol[i]]}
    labelypos <- mean(tmp)

    ## Work out label text
    idx <- names(info)
    if (length(hits) == 1) {
      infodata <- as.character(unlist(info[, idx]))
      infostring <- paste(idx, infodata, collapse = "\n", sep = ": ")
    }
    else {
      xymin <- unlist(lapply(info[, idx], min, na.rm = TRUE))
      xymax <- unlist(lapply(info[, idx], max, na.rm = TRUE))
      infostring <- paste(idx, paste(xymin, xymax, sep = " - "),
                          collapse = "\n", sep = ": ")
      infostring <- paste(" xxhitsxx points\n", infostring)
      infostring <- gsub("xxhitsxx", length(hits), infostring)
    }
    bgwidth <- qstrWidth(painter, infostring)
    bgheight <- qstrHeight(painter, infostring)

    ## adjust drawing directions when close to the boundary
    hflag <- meta$datarange[2] - xpos > bgwidth
    vflag <- ypos - meta$datarange[3] > bgheight
    qdrawRect(painter, labelxpos, labelypos,
              labelxpos + ifelse(hflag, 1, -1) * bgwidth,
              labelypos + ifelse(vflag, -1, 1) * bgheight,
              stroke = rgb(1, 1, 1),
              fill = rgb(1, 1, 1, 0.9))

    qstrokeColor(painter) <- b$label.color
    qdrawText(painter, infostring, labelxpos, labelypos,
              halign = ifelse(hflag, "left", "right"),
              valign = ifelse(vflag, "top", "bottom"))

  }

  query_hover <- function(item, event, ...) {
    meta$query.pos <- as.numeric(event$pos())
    qupdate(query_layer)
  }

  query_hover_leave <- function(item, event, ...) {
    meta$query.pos <- NULL
    qupdate(query_layer)
  }

############
  ## layers ##----------
############

  xaxis <- function(layer, painter) {
    sx <- axis_loc(meta$datarange[1:2])
    draw_x_axes_with_labels_fun(painter, c(meta$datarange[1:2],1,5),
                                axisLabel =sx, labelHoriPos = sx,
                                name = meta$xlab)
  }

  yaxis <- function(layer, painter) {
    if (is.null(meta$group) | !meta$vertconst){
      sy <- axis_loc(meta$datarange[3:4])
    } else {
      sy <- (as.integer(unique(meta$group))-1)*meta$vertconst
    }
    if (!meta$vertconst) {aL <- sy} else {aL <- unique(meta$group)}
    draw_y_axes_with_labels_fun(painter, c(1,5, meta$datarange[3:4]),
                                axisLabel = aL, labelVertPos = sy,
                                name = ifelse(!meta$vertconst,meta$ylab,meta$varname$g))
  }

  grid <- function(layer, painter) {
    sx <- axis_loc(meta$datarange[1:2])
    sy <- axis_loc(meta$datarange[3:4])
    draw_grid_with_positions_fun(painter, meta$datarange, sx, sy)
  }

  main_circle_draw <- function(layer,painter){
    for (j in 1:ncol(meta$y)) {
      color=gray(seq(0,0.6,length=max(meta$wrap.group,na.rm=TRUE)))
      for (k in unique(meta$vargroup)) {
        for (i in 1:max(meta$wrap.group,na.rm=TRUE)) {
          if (sum(meta$wrap.group==i & meta$vargroup==k)){
            qdrawCircle(painter,
                        meta$xtmp[meta$wrap.group==i & meta$vargroup==k],
                        meta$ytmp[meta$wrap.group==i & meta$vargroup==k,j],
                        r=meta$radius,
                        fill=alpha(color[max(meta$wrap.group,na.rm=TRUE)+1-i],meta$alpha),
                        stroke=alpha(color[max(meta$wrap.group,na.rm=TRUE)+1-i],meta$alpha))
          }
        }
      }
    }
  }

  main_line_draw <- function(layer,painter){
    for (j in 1:ncol(meta$y)) {
      color=gray(seq(0,0.6,length=max(meta$wrap.group,na.rm=TRUE)))
      for (k in unique(meta$vargroup)) {
        for (i in 1:max(meta$wrap.group,na.rm=TRUE)) {
          if (sum(meta$wrap.group==i & meta$vargroup==k)){
            qdrawLine(painter,
                      meta$xtmp[meta$wrap.group==i & meta$vargroup==k],
                      meta$ytmp[meta$wrap.group==i & meta$vargroup==k,j],
                      stroke=alpha(color[max(meta$wrap.group,na.rm=TRUE)+1-i],meta$alpha))
          }
        }
      }
    }
  }

  brush_draw <- function(layer, painter) {
       
    if (any(is.na(meta$pos))) return()
    qlineWidth(painter) <- b$style$linewidth
    qdrawRect(painter, meta$pos[1] - meta$brush.size[1],
              meta$pos[2] - meta$brush.size[2], meta$pos[1], meta$pos[2],
              stroke = b$style$color)
    qdrawCircle(painter, meta$pos[1], meta$pos[2],
                r = 1.5 * b$style$linewidth,
                stroke = b$style$color, fill = b$style$color)    
    .brushed <- selected(data)
    if (!any(.brushed)) return()
    hdata <- subset(data.frame(meta$xtmp,meta$ytmp[,unique(meta$hitscol)]), .brushed)
    ## (re)draw brushed data points
    brushx <- hdata[,1]
    brushy <- hdata[,-1]
    shadowmatrix <- matrix(FALSE,nrow=nrow(data),ncol=ncol(meta$y))
    for (i in 1:length(meta$hitsrow)) {
      shadowmatrix[meta$hitsrow[i],meta$hitscol[i]] <- TRUE
    }
    fill <- b$color
    stroke <- b$color
    radius <- meta$radius

    if (is.vector(brushy)){
      qdrawCircle(painter, x = brushx, y = brushy,
                  r = meta$radius*2, fill = fill,
                  stroke = stroke)
    } else {
      for (i in 1:ncol(meta$y)){
        if (any(shadowmatrix[,i])) {
          qdrawCircle(painter, x = meta$xtmp[shadowmatrix[,i]],
                      y = meta$ytmp[shadowmatrix[,i],i],
                      r = meta$radius*2, fill = fill,
                      stroke = stroke)
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
                        limits = qrect(meta$datarange[1:2], c(0, 1)),
                        row=2, col=1, clip=FALSE)
  yaxis_layer <- qlayer(parent=root_layer, paintFun = yaxis,
                        limits = qrect(c(0, 1), meta$datarange[3:4]),
                        row=1, col=0, clip=FALSE)
  bg_layer <- qlayer(parent= root_layer, paintFun = grid,
                     limits = meta$lims, row=1, col=1, clip=FALSE)
  main_circle_layer <- qlayer(root_layer,paintFun=main_circle_draw,
                              mousePressFun=brush_mouse_press, mouseReleaseFun=brush_mouse_move,
                              mouseMove = brush_mouse_move, keyPressFun=key_press,
                              focusInFun = function(...) {focused(data) <- TRUE},
                              focusOutFun = function(...) {focused(data) <- FALSE},
                              limits=meta$lims, row = 1, col = 1, clip=FALSE)
  main_line_layer <- qlayer(root_layer,paintFun=main_line_draw,
                            limits=meta$lims, row = 1, col = 1, clip=FALSE)
  brush_layer <- qlayer(root_layer, brush_draw,
                        limits=meta$lims, row = 1, col = 1, clip=FALSE)
  query_layer <- qlayer(root_layer, query_draw,
                        limits = meta$lims, hoverMoveFun = query_hover,
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
  view$setWindowTitle(main)

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
  attr(view, 'meta') <- meta
  view
}

Time.meta =
    setRefClass("Time_meta", fields =
                signalingFields(list(varname = 'list',
                                     time = 'numeric',
                                     y = 'data.frame',
                                     yorig = 'data.frame',
                                     group = 'factor',
                                     xtmp = 'numeric',
                                     ytmp = 'data.frame',
                                     wrap.group = 'numeric',
                                     wrap.shift = 'numeric',
                                     vargroup = 'numeric',
                                     xlab = 'character',
                                     ylab = 'character',
                                     zoomsize = 'numeric',
                                     datarange = 'numeric',
                                     radius = 'numeric',
                                     alpha = 'numeric',
                                     stroke = 'character',
                                     fill = 'character',
                                     start = 'numeric',
                                     pos = 'numeric',
                                     brush.move = 'logical',
                                     brush.size = 'numeric',
                                     query.pos = 'numeric')))
