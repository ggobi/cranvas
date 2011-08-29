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
##' @family plots


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
  
  ## Period for time series / Group for panel data
  if (is.null(call$period) & is.null(call$group)) {
    meta$vargroup <- rep(1, nrow(data))
    meta$orderEnter <- order(meta$time, decreasing=FALSE)
    meta$orderBack <- rank(meta$time,ties.method='first')
  } else {
    if (!is.null(call$period)) {
      meta$varname$g <- as.character(call$period)
    } else {
      meta$varname$g <- as.character(call$group)
    }
    meta$orderEnter <- order(as.factor(data[,meta$varname$g]), meta$time, decreasing=FALSE)
    meta$group <- factor(data[meta$orderEnter,meta$varname$g])
    meta$orderBack <- rank(meta$time+as.integer(as.factor(data[,meta$varname$g]))*(max(meta$time,na.rm=TRUE)+1),
                           ties.method='first')

    meta$vargroup <- meta$group
  }
  if (!all(meta$orderEnter==1:nrow(data))) {
    meta$time <- meta$time[meta$orderEnter]
    meta$xtmp <- meta$xtmp[meta$orderEnter]
  }
  if (!is.null(call$period)) {
    pdLen <- tapply(meta$time,meta$group,length)
    if (!all(pdLen==pdLen[1])) {
      warning('Period lengths are not the same.')
    }
    ## need to be modified here !!
    meta$time <- rep(1:pdLen[1],length=length(meta$time)) 
    meta$xtmp <- meta$time
  }
    
  ## Y axis setting
  if (inherits(y, 'formula')){
    if (length(y)!=2){stop("Wrong formula format.")}
    meta$varname$y <- all.vars(y)
  } else {
    meta$varname$y <- as.character(call$y) 
  }
  meta$yorig <- as.data.frame(data[meta$orderEnter,meta$varname$y,drop=FALSE])
  meta$y <- meta$yorig
  if (ncol(meta$yorig)>1) {
    for (i in 1:ncol(meta$yorig)) {
      meta$y[,i] <- (meta$yorig[,i] - min(meta$yorig[,i], na.rm = TRUE))/
        diff(range(meta$yorig[,i], na.rm = TRUE))
    }
  }
  meta$ytmp <- meta$y
  meta$ylab <- ifelse(is.null(ylab), paste(meta$varname$y,collapse=', '), ylab)

  ## Other settings
  meta$wrap.mode <- wrap
  meta$wrap.group <- rep(1, nrow(data))
  meta$wrap.shift <- shift
  meta$wrapF_dragT <- FALSE
  meta$hitscol <- 1
  meta$hitsrow <- NULL
  meta$vertconst <- 0

  ## Range etc.
  meta$zoomsize <- diff(range(meta$xtmp, na.rm = TRUE))
  meta$limits <- matrix(c(extend_ranges(meta$xtmp),
                        extend_ranges(range(meta$ytmp, na.rm = TRUE))), nrow=2)
  meta$xat <- axis_loc(meta$limits[1:2])
  meta$yat <- axis_loc(meta$limits[3:4])
  meta$xlabels <- format(meta$xat)
  meta$ylabels <- format(meta$yat)
  meta$shiftUP <- FALSE
  meta$shiftDOWN <- FALSE

  ## Radius etc.
  meta$radius <- size
  meta$alpha <- alpha
  meta$stroke <- data$.border[meta$orderEnter]
  meta$fill <- data$.color[meta$orderEnter]
  meta$serie.mode <- FALSE
  meta$serie.pos <- NULL

  ## Brush etc.
  meta$pos <- c(NA, NA)
  meta$query.pos <- NULL
  meta$start <- c(NA, NA)
  meta$brush.move <- TRUE
  meta$brush.size <- c(diff(meta$limits[1:2]),
                       diff(meta$limits[3:4]))/30

  ## Title
  meta$main <- if (is.null(main)) 
                 sprintf("Time Plot of %s And %s", 
                         meta$varname$x, paste(meta$varname$y, collapse=', ')) else main
  
  ## set limits for yaxis
  meta.yaxis <- function() {
    if (meta$shiftUP) {
      meta$yat <- 1:ncol(meta$y)+0.5
      meta$ylabels <- meta$varname$y
      meta$ylab <- ""
      #meta$shiftUP <- FALSE
    } else if (meta$shiftDOWN) {
      meta$yat <- axis_loc(meta$limits[3:4])
      meta$ylabels <- format(meta$yat)
      meta$ylab <- paste(meta$varname$y,collapse=', ')
      meta$shiftUP <- FALSE
    } else {
      if (is.null(meta$group) | !meta$vertconst){
        meta$yat <- axis_loc(meta$limits[3:4])
      } else {
        meta$yat <- (as.integer(unique(meta$group))-1)*meta$vertconst+0.5
      }
      if (meta$vertconst==0) {
        meta$ylabels <- format(meta$yat)
        meta$ylab <- ifelse(is.null(ylab), paste(meta$varname$y,collapse=', '), ylab)
      } else {
        meta$ylabels <- format(unique(meta$group))
        meta$ylab <- meta$varname$g
      }
    }
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
      if (meta$serie.mode | meta$wrapF_dragT) {
        b$cursor <- 18L
      } else {
        b$cursor <- 0L
      }    
    }
  }

  brush_mouse_move <- function(layer, event) {
    if (event$button() != Qt$Qt$NoButton) {
      b$cursor <- 0L
    }
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
    
    selected(data) <- meta$orderEnter[meta$hitsrow]
    if (!is.null(meta$group)) self_link(data)
    
    if (meta$wrapF_dragT) {
      meta$limits[1:2] <- meta$limits[1:2] - meta$pos[1] + meta$start[1]
      if (meta$limits[1,1]<extend_ranges(meta$time)[1]) {
        meta$limits[1:2] <- meta$limits[1:2] - meta$limits[1,1] + extend_ranges(meta$time)[1]
      } else if (meta$limits[2,1]>extend_ranges(meta$time)[2]) {
        meta$limits[1:2] <- meta$limits[1:2] - meta$limits[2,1] + extend_ranges(meta$time)[2]
      }
      meta$xat <- axis_loc(meta$limits[1:2])
      meta$xlabels <- format(meta$xat)
      
      qupdate(main_circle_layer)
      qupdate(main_line_layer)
    }
  }

  key_press <- function(layer, event){
    crt_range <- diff(range(meta$xtmp,na.rm=TRUE))+1
    
    if (event$key()==Qt$Qt$Key_W){
      ## key W for switching the wrapping mode
      meta$wrap.mode <- !meta$wrap.mode
      qupdate(layer.WRAPtext)
    } else if (event$key()==Qt$Qt$Key_M){
      ## key M for switching the serie mode (for selecting and moving)     
      if (length(meta$group)){
        meta$serie.mode <- !meta$serie.mode
        if (!meta$serie.mode) {
          link_type(data) <- NULL
        } else {
          if (class(data[,meta$varname$g])=='factor'){
            link_var(data) <- meta$varname$g
            link_type(data) <- "self"
          } else {
            message("The group variable is not a factor. Please change to factor before pressing M.")
            meta$serie.mode <- FALSE
          }
        }
      }
      if (!meta$wrap.mode) {
        if (meta$limits[1,1]>extend_ranges(meta$time)[1] | meta$limits[2,1]<extend_ranges(meta$time)[2]) {
          meta$wrapF_dragT <- !meta$wrapF_dragT
        }
      }
    } else if (event$key()==Qt$Qt$Key_G){
      ## key G for gear(shift the wrapping speed)
      
      meta$wrap.shift <- c(meta$wrap.shift[-1],meta$wrap.shift[1])
      qupdate(layer.WRAPtext)
    } else if (event$key()==Qt$Qt$Key_Right){
      ## arrow right

      if (meta$wrap.mode) {
        if (meta$serie.mode & sum(selected(data))) {
          meta$xtmp[selected(data)] <- meta$xtmp[selected(data)] + diff(range(meta$time,na.rm=TRUE))/30
          if (min(meta$xtmp[selected(data)],na.rm=TRUE)>max(meta$time,na.rm=TRUE)) {
            meta$xtmp[selected(data)] <- meta$xtmp[selected(data)] - 
              min(meta$xtmp[selected(data)],na.rm=TRUE) + max(meta$time,na.rm=TRUE)
          }
        } else if (is.null(call$period) & is.null(call$group) & event$modifiers() == Qt$Qt$ShiftModifier) {
          zoombound <- max(meta$wrap.shift)
          if (zoombound<2) zoombound <- diff(range(meta$x,na.rm=TRUE))/4
          meta$xtmp <- meta$time %% zoombound
          meta$wrap.group <- ceiling(meta$time/zoombound)
          if (sum(meta$xtmp==0)){
            meta$wrap.group[meta$xtmp==0] <- meta$wrap.group[which(meta$xtmp==0)-1]
            meta$xtmp[meta$xtmp==0] <- zoombound
          }
          meta$limits[1:2] <- extend_ranges(meta$xtmp)
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
          meta$limits[1:2] <- extend_ranges(meta$xtmp)
        }
      } else {
        if (meta$wrapF_dragT) {
          meta$limits[1:2] <- meta$limits[1:2] + diff(range(meta$time,na.rm=TRUE))/30
          if (max(meta$limits[1:2])>max(meta$time,na.rm=TRUE)) {
            meta$limits[1:2] <- meta$limits[1:2] - max(meta$limits[1:2]) + max(meta$time,na.rm=TRUE)
          }
        } else {
        meta$zoomsize <- meta$zoomsize-2
        if (meta$zoomsize < 2) meta$zoomsize <- 2
        tmpXzoom <- meta$start[1] + c(-0.5,0.5) * meta$zoomsize
        tmpXzoom[1] <- max(tmpXzoom[1], min(meta$time, na.rm=TRUE))
        tmpXzoom[2] <- min(tmpXzoom[2], max(meta$time, na.rm=TRUE))
        meta$limits[1:2] <- extend_ranges(tmpXzoom)
        #meta$xtmp <- meta$time
        #meta$xtmp[meta$time<=tmpXzoom[1]]=NA
        #meta$xtmp[meta$time>=tmpXzoom[2]]=NA
        }
      }
      meta$xat <- axis_loc(meta$limits[1:2])
      meta$xlabels <- format(meta$xat)
    } else if (event$key()==Qt$Qt$Key_Left){
      ## arrow left
      
      if (event$modifiers() == Qt$Qt$ShiftModifier) {
          meta$xtmp <- meta$time
          meta$wrap.group <- 1
          meta$zoomsize <- diff(range(meta$xtmp, na.rm = TRUE))
          meta$limits[1:2] <- extend_ranges(meta$xtmp)
      } else {
      if (meta$wrap.mode) {
        if (meta$serie.mode & sum(selected(data))) {
          meta$xtmp[selected(data)] <- meta$xtmp[selected(data)] - diff(range(meta$time,na.rm=TRUE))/30
          if (max(meta$xtmp[selected(data)],na.rm=TRUE)<min(meta$time,na.rm=TRUE)) {
            meta$xtmp[selected(data)] <- meta$xtmp[selected(data)] - 
              max(meta$xtmp[selected(data)],na.rm=TRUE) + min(meta$time,na.rm=TRUE)
          }
        } else {
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
          zoombound <- zoombound+max(meta$wrap.shift)
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
        }
        meta$limits[1:2] <- extend_ranges(meta$xtmp)       
      } else {
        if (meta$wrapF_dragT) {
          meta$limits[1:2] <- meta$limits[1:2] - diff(range(meta$time,na.rm=TRUE))/30
          if (min(meta$limits[1:2])<min(meta$time,na.rm=TRUE)) {
            meta$limits[1:2] <- meta$limits[1:2] - min(meta$limits[1:2]) + min(meta$time,na.rm=TRUE)
          }
        } else {
          meta$zoomsize <- meta$zoomsize+2
          if (meta$zoomsize > 2*diff(range(meta$time,na.rm=TRUE))) {
            meta$zoomsize <- 2*diff(range(meta$time,na.rm=TRUE))
          }
          tmpXzoom <- meta$start[1] + c(-0.5,0.5) * meta$zoomsize
          tmpXzoom[1] <- max(tmpXzoom[1], min(meta$time, na.rm=TRUE))
          tmpXzoom[2] <- min(tmpXzoom[2], max(meta$time, na.rm=TRUE))
          meta$limits[1:2] <- extend_ranges(tmpXzoom)
          #meta$xtmp <- meta$time
          #meta$xtmp[meta$time<tmpXzoom[1]]=NA
          #meta$xtmp[meta$time>tmpXzoom[2]]=NA
        }
      }
      }
	  meta$xat <- axis_loc(meta$limits[1:2])
    meta$xlabels <- format(meta$xat)

    } else if (event$key() == Qt$Qt$Key_U) {
        ## Key U (for Up)
        
        if (ncol(meta$y)>1 & event$modifiers() == Qt$Qt$ShiftModifier) {
          for (i in 1:ncol(meta$y)){
            meta$ytmp[,i] <- meta$y[,i]+i
          }
          meta$shiftUP <- TRUE
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
        }
        meta$limits[3:4] <-  extend_ranges(range(meta$ytmp,na.rm=TRUE))
        meta.yaxis()
    } else if (event$key() == Qt$Qt$Key_D) {
        ## Key D (for Down)
        meta$shiftUP <- FALSE
        if (ncol(meta$y)>1 & event$modifiers() == Qt$Qt$ShiftModifier) {
          meta$ytmp <- meta$y
          meta$shiftDOWN <- TRUE
        } else {
          if (!is.null(meta$group) & length(meta$group)>0) {
            meta$vertconst <- meta$vertconst - 0.05
            if (meta$vertconst<0) meta$vertconst <- 0
            if (!meta$vertconst) {
              meta$ytmp <- meta$y
              meta$limits[3:4] <-  extend_ranges(range(meta$ytmp,na.rm=TRUE))      
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
          }
        } 
          meta$limits[3:4] <-  extend_ranges(range(meta$ytmp,na.rm=TRUE))  
          meta.yaxis()
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
    xrange <- meta$radius/layer.root$size$width() *
      diff(meta$limits[c(1, 2)]) * queryaround
    yrange <- meta$radius/layer.root$size$height() *
      diff(meta$limits[c(3, 4)]) * queryaround

    rect <- qrect(matrix(c(xpos - xrange, ypos - yrange,
                           xpos + xrange, ypos + yrange),
                         2, byrow = TRUE))
    main_circle_layer$invalidateIndex()
    main_line_layer$invalidateIndex()
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
    info <- as.data.frame(data[meta$orderEnter[hitsrow],
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
    hflag <- meta$limits[2] - xpos > bgwidth
    vflag <- ypos - meta$limits[3] > bgheight
    qdrawRect(painter, labelxpos, labelypos,
              labelxpos + ifelse(hflag, 1, -1) * bgwidth,
              labelypos + ifelse(vflag, -1, 1) * bgheight,
              stroke = rgb(1, 1, 1),
              fill = rgb(1, 1, 1, 0.9))

    qstrokeColor(painter) <- b$label.color
    qdrawText(painter, infostring, labelxpos, labelypos,
              halign = ifelse(hflag, "left", "right"),
              valign = ifelse(vflag, "top", "bottom"))
    
    if (meta$serie.mode){
      selected(data) <- meta$orderEnter[hitsrow]
      self_link(data)
      if ("self" %in% link_type(data) & (!is.null(link_var(data)))) {
        meta$hitsrow <- meta$orderBack[selected(data)]
        meta$hitscol <- rep(as.integer(names(sort(table(meta$hitscol),decreasing=TRUE))[1]),length(meta$hitsrow))
      }
      shadowmatrix <- matrix(FALSE,nrow=nrow(data),ncol=ncol(meta$y))
      for (i in 1:length(meta$hitsrow)) {
        shadowmatrix[meta$hitsrow[i],meta$hitscol[i]] <- TRUE
      }
      fill <- b$color
      stroke <- b$color
      radius <- meta$radius
      for (i in 1:ncol(meta$y)){
        if (any(shadowmatrix[,i])) {
          qdrawCircle(painter, x = meta$xtmp[shadowmatrix[,i]],
                      y = meta$ytmp[shadowmatrix[,i],i],
                      r = meta$radius*2, fill = fill,
                      stroke = stroke)
          if (sum(shadowmatrix[,i])>1) {
            for (k in unique(meta$vargroup)) {
              for (j in 1:max(meta$wrap.group,na.rm=TRUE)) {
                idxtmp <- (meta$wrap.group==j & meta$vargroup==k & shadowmatrix[,i])
                if (sum(idxtmp)){                 
                  xtmp <- meta$xtmp
                  ytmp <- meta$ytmp[,i]
                  xtmp[!idxtmp] <- NA
                  ytmp[!idxtmp] <- NA
                  qdrawLine(painter, xtmp, ytmp, stroke=stroke)
                }
              }
            }
          }
        }
      }
    }

  }

  query_hover <- function(item, event, ...) {
    meta$query.pos <- as.numeric(event$pos())
    if (meta$serie.mode) meta$serie.pos <- as.numeric(event$pos())
    qupdate(query_layer)
  }

  query_hover_leave <- function(item, event, ...) {
    meta$query.pos <- NULL
    if (meta$serie.mode) meta$serie.pos <- NULL
    qupdate(query_layer)
  }

############
  ## layers ##----------
############

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

    if (meta$serie.mode) {
      if (is.null(meta$serie.pos)) return()
      xpos <- meta$start[1]
      ypos <- meta$start[2]
      queryaround <- ifelse(meta$radius<=4,8/meta$radius,1)
      xrange <- meta$radius/layer.root$size$width() *
                diff(meta$limits[c(1, 2)]) * queryaround
      yrange <- meta$radius/layer.root$size$height() *
                diff(meta$limits[c(3, 4)]) * queryaround
      rect <- qrect(matrix(c(xpos - xrange, ypos - yrange,
                             xpos + xrange, ypos + yrange),
                           2, byrow = TRUE))
      main_circle_layer$invalidateIndex()
      main_line_layer$invalidateIndex()
      hits <- main_circle_layer$locate(rect) + 1
      if (length(hits) < 1) {
        selected(data) <- FALSE
        return()
      }
      hitsrow <- round(hits %% nrow(data))
      hitsrow[hitsrow==0] <- nrow(data)
      hitscol <- (hits-0.0000001)%/%nrow(data)+1
      selected(data) <- meta$orderEnter[hitsrow]
      self_link(data)
      if ("self" %in% link_type(data) & (!is.null(link_var(data)))) {
        meta$hitsrow <- meta$orderBack[selected(data)]
        meta$hitscol <- rep(as.integer(names(sort(table(meta$hitscol),decreasing=TRUE))[1]),length(meta$hitsrow))
      }
      shadowmatrix <- matrix(FALSE,nrow=nrow(data),ncol=ncol(meta$y))
      for (i in 1:length(meta$hitsrow)) {
        shadowmatrix[meta$hitsrow[i],meta$hitscol[i]] <- TRUE
      }
      fill <- b$color
      stroke <- b$color
      radius <- meta$radius
      #meta$xtmp <- meta$time 
      for (i in 1:ncol(meta$y)){
        if (any(shadowmatrix[,i])) {
          meta$xtmp[shadowmatrix[,i]] <- meta$xtmp[shadowmatrix[,i]] + meta$pos[1] - meta$start[1]
          qdrawCircle(painter, x = meta$xtmp[shadowmatrix[,i]],
                      y = meta$ytmp[shadowmatrix[,i],i],
                      r = meta$radius*2, fill = fill,
                      stroke = stroke)
          if (sum(shadowmatrix[,i])>1) {
            for (k in unique(meta$vargroup)) {
              for (j in 1:max(meta$wrap.group,na.rm=TRUE)) {
                idxtmp <- (meta$wrap.group==j & meta$vargroup==k & shadowmatrix[,i])
                if (sum(idxtmp)){                 
                  xtmp <- meta$xtmp
                  ytmp <- meta$ytmp[,i]
                  xtmp[!idxtmp] <- NA
                  ytmp[!idxtmp] <- NA
                  qdrawLine(painter, xtmp, ytmp, stroke=stroke)
                }
              }
            }
          }
        }
      }
      qupdate(main_circle_layer)
      qupdate(main_line_layer)
      return()
    }
    
    if (meta$wrapF_dragT) {     
      qupdate(main_circle_layer)
      qupdate(main_line_layer)
      return()
    }
    
    qlineWidth(painter) <- b$style$linewidth
    qdrawRect(painter, meta$pos[1] - meta$brush.size[1],
              meta$pos[2] - meta$brush.size[2], meta$pos[1], meta$pos[2],
              stroke = b$style$color)
    qdrawCircle(painter, meta$pos[1], meta$pos[2],
                r = 1.5 * b$style$linewidth,
                stroke = b$style$color, fill = b$style$color)    
    if (!any(selected(data))) return()
    if ("self" %in% link_type(data) & (!is.null(link_var(data)))) {
      meta$hitsrow <- meta$orderBack[selected(data)]
      meta$hitscol <- rep(as.integer(names(sort(table(meta$hitscol),decreasing=TRUE))[1]),length(meta$hitsrow))
    }
    shadowmatrix <- matrix(FALSE,nrow=nrow(data),ncol=ncol(meta$y))
    for (i in 1:length(meta$hitsrow)) {
      shadowmatrix[meta$hitsrow[i],meta$hitscol[i]] <- TRUE
    }
    fill <- b$color
    stroke <- b$color
    radius <- meta$radius

    for (i in 1:ncol(meta$y)){
      if (any(shadowmatrix[,i])) {
         qdrawCircle(painter, x = meta$xtmp[shadowmatrix[,i]],
                    y = meta$ytmp[shadowmatrix[,i],i],
                    r = meta$radius*2, fill = fill,
                    stroke = stroke)
         if (sum(shadowmatrix[,i])>1) {
           for (k in unique(meta$vargroup)) {
             for (j in 1:max(meta$wrap.group,na.rm=TRUE)) {
               idxtmp <- (meta$wrap.group==j & meta$vargroup==k & shadowmatrix[,i])
               if (sum(idxtmp)){                 
                 xtmp <- meta$xtmp
                 ytmp <- meta$ytmp[,i]
                 xtmp[!idxtmp] <- NA
                 ytmp[!idxtmp] <- NA
                 qdrawLine(painter, xtmp, ytmp, stroke=stroke)
               }
            }
          }
        }
      }
    }  
  }
  
  ACF_draw <- function(layer, painter){
    j <- is.null(call$period) & is.null(call$group)
    if (!j) return()
    if (ncol(meta$ytmp)>1) {
      ytmpacf <- round(unlist(lapply(apply(meta$ytmp,2,acf,plot=F),function(z)z$acf[meta$wrap.shift[1]+1])),2)
      if (meta$shiftUP) {
        qdrawText(painter,paste(meta$varname$y,": ACF(lag=",meta$wrap.shift[1],"):",
                                ytmpacf,sep=""),
                  rep(meta$limits[1,1],ncol(meta$ytmp)),meta$yat-0.5, 
                  halign='left',valign='bottom')
      } else {
        qdrawText(painter,paste(paste(meta$varname$y,": ACF(lag=",meta$wrap.shift[1],"):",
                                ytmpacf,sep=""),collapse="\n"),
                  meta$limits[1,1],meta$limits[1,2], 
                  halign='left',valign='bottom')
      }
    } else{
      qdrawText(painter,paste("ACF(lag=",meta$wrap.shift[1],"):",
                              round(acf(meta$ytmp,na.action=na.pass,plot=FALSE)$acf[meta$wrap.shift[1]+1],2),
                              sep=""),
                meta$limits[1,1],meta$limits[1,2], 
                halign='left',valign='bottom')
    }
  }

#####################
  ## draw the canvas ##----------
#####################

  xWidth <- 800
  yWidth <- 500
  if (!is.null(asp)) xWidth <- round(yWidth*asp)

  scene <- qscene()
  layer.root <- qlayer(scene)
  layer.title <- qmtext(meta = meta, side = 3)
  layer.xlab = qmtext(meta = meta, side = 1)
  layer.ylab = qmtext(meta = meta, side = 2)
  layer.xaxis = qaxis(meta = meta, side = 1)
  layer.yaxis = qaxis(meta = meta, side = 2)
  layer.grid = qgrid(meta = meta, minor = 'xy')
  main_circle_layer <- qlayer(paintFun=main_circle_draw,
                              mousePressFun=brush_mouse_press, mouseReleaseFun=brush_mouse_move,
                              mouseMove = brush_mouse_move, keyPressFun=key_press,
                              focusInFun = function(...) {focused(data) <- TRUE},
                              focusOutFun = function(...) {focused(data) <- FALSE},
                              limits=qrect(meta$limits),clip=TRUE)
  main_line_layer <- qlayer(paintFun=main_line_draw,
                            limits=qrect(meta$limits),clip=TRUE)
  brush_layer <- qlayer(paintFun=brush_draw,
                        limits=qrect(meta$limits),hoverMoveFun = query_hover,
                        hoverLeaveFun = query_hover_leave)
  query_layer <- qlayer(paintFun=query_draw,
                        limits =qrect(meta$limits), hoverMoveFun = query_hover,
                        hoverLeaveFun = query_hover_leave)
  layer.WRAPtext <- qlayer(paintFun=function(layer,painter){
                           if (!meta$wrap.mode) return()
                           qdrawText(painter,paste("Wrapping Period:",meta$wrap.shift[1]),
                                                                  meta$limits[2,1],meta$limits[1,2],
                                                                  halign='right',valign='bottom')},
                       limits=qrect(meta$limits))
  layer.ACFtext <- qlayer(paintFun=ACF_draw, limits=qrect(meta$limits))
  
  layer.root[0, 2] = layer.title
  layer.root[2, 2] = layer.xaxis
  layer.root[3, 2] = layer.xlab
  layer.root[1, 1] = layer.yaxis
  layer.root[1, 0] = layer.ylab
  layer.root[1, 2] = layer.grid
  layer.root[1, 2] = main_circle_layer
  layer.root[1, 2] = main_line_layer
  layer.root[1, 2] = brush_layer
  layer.root[1, 2] = query_layer
  layer.root[1, 2] = layer.WRAPtext
  j <- is.null(call$period) & is.null(call$group)
  if (j) layer.root[1, 2] = layer.ACFtext
  layer.root[1, 3] = qlayer() 
  layout = layer.root$gridLayout()
  layout$setRowPreferredHeight(0, 30)
  layout$setRowPreferredHeight(2, 15 * max(sapply(gregexpr('\\n', meta$xlabels),
                              function(xx) ifelse(any(xx <0), 0, length(xx)) + 2)))
  layout$setRowPreferredHeight(3, 20)
  layout$setColumnPreferredWidth(0, 10)
  layout$setColumnPreferredWidth(1, 9 * max(nchar(unlist(strsplit(meta$ylabels, '\n')))) + 5)
  layout$setColumnMaximumWidth(3, 10)
  layout$setRowStretchFactor(0, 0)
  layout$setRowStretchFactor(2, 0)
  layout$setRowStretchFactor(3, 0)
  layout$setColumnStretchFactor(0, 0)
  layout$setColumnStretchFactor(1, 0)

  view <- qplotView(scene=scene)
  view$setWindowTitle(meta$main)

######################
  ## add some listeners #
######################
  ## if (is.mutaframe(data)) {
  d.idx = add_listener(data, function(i, j) {
        switch(j, .brushed = qupdate(brush_layer),
               .color = {
                   qupdate(main_circle_layer)
                   qupdate(main_line_layer)
               }, {
                   qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
                   main_circle_layer$invalidateIndex()
                   main_line_layer$invalidateIndex()
                   qupdate(main_circle_layer)
                   qupdate(main_line_layer)
               })
  })
  qconnect(main_circle_layer, 'destroyed', function(x) {
        ## b$colorChanged$disconnect(b.idx)
        remove_listener(data, d.idx)
  })
  b$cursorChanged$connect(function() {
        set_cursor(view, b$cursor)
  })                          
  sync_limits(meta, main_circle_layer,main_line_layer,
              query_layer, brush_layer,
              layer.WRAPtext,layer.ACFtext)
  meta$manual.brush = function(pos) {
    brush_mouse_move(layer = main_circle_layer, event = list(pos = function() pos))
  }
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
                                     orderEnter = 'numeric',
                                     orderBack = 'numeric',
                                     xtmp = 'numeric',
                                     ytmp = 'data.frame',
                                     wrap.mode = 'logical',  
                                     wrap.group = 'numeric',
                                     wrap.shift = 'numeric',
                                     wrapF_dragT = 'logical',
                                     vargroup = 'numeric',
                                     xat = 'numeric',
                                     yat = 'numeric',
                                     xlabels = 'character',
                                     ylabels = 'character',
                                     xlab = 'character',
                                     ylab = 'character',
                                     main = 'character',
                                     zoomsize = 'numeric',
                                     limits = 'matrix',
                                     radius = 'numeric',
                                     alpha = 'numeric',
                                     stroke = 'character',
                                     fill = 'character',
                                     start = 'numeric',
                                     pos = 'numeric',
                                     brush.move = 'logical',
                                     brush.size = 'numeric',
                                     query.pos = 'numeric',
                                     serie.mode = 'logical',
                                     serie.pos = 'numeric')))
