Time.meta =
  setRefClass("Time_meta", fields =
  properties(c(Common.meta,
               list(varname = 'list',
                    time = 'numeric',
                    y = 'numeric',
                    yorig = 'data.frame',
                    group = 'factor',
                    orderEnter = 'numeric',
                    orderBack = 'numeric',
                    xtmp = 'numeric',
                    ytmp = 'numeric',
                    shadow.matrix = 'data.frame',
                    wrap.mode = 'logical',  
                    wrap.group = 'numeric',
                    wrap.shift = 'numeric',
                    wrapF_dragT = 'logical',
                    vargroup = 'factor',
                    zoomsize = 'numeric',
                    limits = 'matrix',
                    radius = 'numeric',
                    stroke = 'character',
                    fill = 'character',
                    query.pos = 'numeric',
                    serie.mode = 'logical',
                    serie.pos = 'numeric'))))


##' Create data for drawing time plots
##' 
##' @param data a data frame for time plot
##' @param y a vertor of all the variable names of interest
##' @inheritParams qdata
##' @return A mutaframe of multiple y's
##' @export
##' @examples 
##' library(cranvas); data(nasa)
##' nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
##' qnasa <- time_qdata(nasa11,c("ts","ps_tovs","ca_med"))
##'
time_qdata <- function(data, y, color = "gray15", border = color, size = 4, brushed = FALSE, 
                       visible = TRUE, copy = TRUE) {
  ycol <- length(y)
  data$.row <- 1:nrow(data)
  newdat <- data.frame(.variable=rep(y[1],nrow(data)),.value=data[,y[1]],data)
  newdat[,y[1]] <- TRUE
  newdat[,y[-1]] <- FALSE
  if (ycol > 1) {      
    for (i in 2:ycol) {
      tmpnewdat <- data.frame(.variable=rep(y[i],nrow(data)),.value=data[,y[i]],data)
      tmpnewdat[,y[i]] <- TRUE
      tmpnewdat[,y[-i]] <- FALSE
      newdat <- rbind(newdat, tmpnewdat)
    }
  }
  newdat$.variable <- as.factor(newdat$.variable)
  return(qdata(newdat, color = color, border = border, size = size, brushed = brushed, 
               visible = visible, copy = copy))
}


##' Initialize the Time.meta
##' 
time_meta_initialize <- function(meta,call,data,period, group, wrap,
                                 shift, size, alpha, asp,
                                 main, xlab, ylab,...){
  
  ## X axis setting
  meta$time <- eval(call$time, as.data.frame(data))
  meta$xtmp <- meta$time
  meta$xlab <- ifelse(is.null(xlab), meta$varname$x, xlab)
  meta$singleVarLen <- max(data$.row)
  meta$nyvar <- length(table(data$.variable))
  
  ## Period for time series / Group for panel data
  if (is.null(call$period) & is.null(call$group)) {
    meta$vargroup <- factor(rep(1, nrow(data)))
    meta$orderEnter <- order(meta$time[1:meta$singleVarLen], decreasing=FALSE)
    meta$orderEnter <- rep(meta$orderEnter,meta$nyvar)+
                       rep((0:(meta$nyvar-1))*meta$singleVarLen,each=meta$singleVarLen)
    meta$orderBack <- rank(meta$time[1:meta$singleVarLen],ties.method='first')
    meta$orderBack <- rep(meta$orderBack,meta$nyvar)+
                      rep((0:(meta$nyvar-1))*meta$singleVarLen,each=meta$singleVarLen)
  } else {
    if (!is.null(call$period)) {
      meta$varname$g <- as.character(call$period)
    } else {
      meta$varname$g <- as.character(call$group)
    }
    meta$orderEnter <- order(as.factor(data[1:meta$singleVarLen,meta$varname$g]), 
                             meta$time[1:meta$singleVarLen], decreasing=FALSE)
    meta$orderEnter <- rep(meta$orderEnter,meta$nyvar)+
                       rep((0:(meta$nyvar-1))*meta$singleVarLen,each=meta$singleVarLen)
    meta$orderBack <- rank(meta$time[1:meta$singleVarLen] + 
                           as.integer(as.factor(data[1:meta$singleVarLen,meta$varname$g])) * 
                           (max(meta$time[1:meta$singleVarLen],na.rm=TRUE)+1),
                           ties.method='first')
    meta$orderBack <- rep(meta$orderBack,meta$nyvar)+
                      rep((0:(meta$nyvar-1))*meta$singleVarLen,each=meta$singleVarLen)
    meta$group <- factor(data[meta$orderEnter,meta$varname$g])
    meta$vargroup <- meta$group
  }
  if (!all(meta$orderEnter==1:nrow(data))) {
    meta$time <- meta$time[meta$orderEnter]
    meta$xtmp <- meta$xtmp[meta$orderEnter]
  }
  if (!is.null(call$period)) {
    pdLen <- tapply(meta$time,factor(paste(meta$.variable,meta$group)),length)
    if (!all(pdLen==pdLen[1])) {
      warning('Period lengths are not the same.')
      ## need to be modified here !!
      maxpdLen <- max(pdLen)
      meta$time <- meta$time %% maxpdLen
      meta$time[meta$time==0] <- maxpdLen
    } else {
      meta$time <- rep(1:pdLen[1],length=length(meta$time))
    }
    meta$xtmp <- meta$time
  }
  
  ## Y axis setting
  meta$varname$y <- as.character(unique(data$.variable)) 
  meta$yorig <- as.data.frame(data)[meta$orderEnter,c(".variable",".value")]
  meta$ylist <- as.data.frame(data)[meta$orderEnter,meta$varname$y]
  meta$y <- meta$yorig[,2]
  if (meta$nyvar>1) {
    for (i in 1:meta$nyvar) {
      tmprow <- meta$ylist[,i]
      tmprowdat <- meta$yorig[tmprow,2]
      meta$y[tmprow] <- (tmprowdat - min(tmprowdat, na.rm = TRUE))/
        diff(range(tmprowdat, na.rm = TRUE))
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
  meta$linkID <- NULL
  
  ## Range, axes, etc.
  meta$zoomsize <- diff(range(meta$xtmp, na.rm = TRUE))
  meta$limits <- matrix(c(extend_ranges(meta$xtmp),
                          extend_ranges(range(meta$ytmp, na.rm = TRUE))), nrow=2)
  meta$xat <- axis_loc(meta$limits[1:2])
  meta$yat <- axis_loc(meta$limits[3:4])
  meta$xlabels <- format(meta$xat)
  meta$ylabels <- format(meta$yat)
  meta$shiftUP <- FALSE
  meta$shiftDOWN <- FALSE
  
  ## Radius, color, etc.
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
                       -diff(meta$limits[3:4]))/30
  
  ## Title
  meta$main <- if (is.null(main)) 
    sprintf("Time Plot of %s And %s", 
            meta$varname$x, paste(meta$varname$y, collapse=', ')) else main
  
}


##' Set limits for yaxis in qtime
meta.yaxis <- function() {
  if (meta$shiftUP) {
    meta$yat <- 1:meta$nyvar+0.5
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
      meta$yat <- (as.integer(unique(meta$group))-0.5)*meta$vertconst
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

##'
selected_draw <- function(meta,b,hits,painter){
  qdrawGlyph(painter, qglyphCircle(r = meta$radius*2), meta$xtmp[hits], 
             meta$ytmp[hits], stroke = b$color, fill = b$color)     
  for (i in 1:meta$nyvar){
    for (k in unique(meta$vargroup)) {
      for (j in 1:max(meta$wrap.group,na.rm=TRUE)) {
        idxtmp <- (meta$wrap.group==j & meta$vargroup==k & meta$ylist[,i] & hits)
        if (sum(idxtmp)){                 
          xtmp <- meta$xtmp
          ytmp <- meta$ytmp
          xtmp[!idxtmp] <- NA
          ytmp[!idxtmp] <- NA
          qdrawLine(painter, xtmp, ytmp, stroke=b$color)
        }
      }
    }
  }
}


qtime2 <- function(time, data, period=NULL, group=NULL, wrap=TRUE,
                  shift=c(1,4,7,12,24), size=2, alpha=1, asp=NULL, 
                  main=NULL, xlab=NULL, ylab=NULL,...){
  
  #####################
  ## data processing ##----------
  #####################
  
  data <- check_data(data)
  call <- as.list(match.call()[-1])
  b <- brush(data)
  meta <- Time.meta$new(varname = list(x = as.character(call$time)), minor = 'xy') 
  time_meta_initialize(meta,call,data=data,period=period, group=group, wrap=wrap,
                       shift=shift, size=size, alpha=alpha, asp=asp, 
                       main=main, xlab=xlab, ylab=ylab)
  tree <- createTree(data.frame(x=meta$xtmp,y=meta$ytmp))
  
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
    meta$pos <- as.numeric(event$pos())
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
    rect <- as.matrix(qrect(update_brush_size(meta)))
    hits <- rectLookup(tree, rect[1, ], rect[2, ])
    if (length(hits)<1) {
      selected(data) <- FALSE
      return()
    }
    selected(data) <- meta$orderEnter[hits]
  }
  
  mouse_wheel = function(layer, event) {
    pos <- as.numeric(event$pos())
    lim <- meta$limits
    p <- (pos - lim[1, ]) / (lim[2, ] - lim[1, ])
    meta$limits[1:2] <- extend_ranges(meta$limits[1:2], -sign(event$delta()) * 0.05 * c(p[1], 1 - p[1]))
    tmprange <- extend_ranges(unlist(meta$mxtmp))
    meta$limits[1,1] <- max(meta$limits[1,1],min(tmprange))
    meta$limits[2,1] <- min(meta$limits[2,1],max(tmprange))
    if (meta$limits[1,1]<=min(tmprange) & meta$limits[2,1]>=max(tmprange)){
      meta$wrapF_dragT <- FALSE
    } else {
      meta$wrapF_dragT <- TRUE
      meta$wrap.mode <- FALSE
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
    for (j in 1:meta$nyvar) {
      color=gray(seq(0,0.6,length=max(meta$wrap.group,na.rm=TRUE)))
      for (k in unique(meta$vargroup)) {
        for (i in 1:max(meta$wrap.group,na.rm=TRUE)) {
          if (sum(meta$wrap.group==i & meta$vargroup==k)){
            tmprow <- 1:meta$singleVarLen + meta$singleVarLen * (j-1)
            qdrawGlyph(painter, qglyphCircle(r = meta$radius), 
                       meta$xtmp[meta$wrap.group==i & meta$vargroup==k & 1:length(meta$xtmp) %in% tmprow],
                       meta$ytmp[meta$wrap.group==i & meta$vargroup==k & 1:length(meta$ytmp) %in% tmprow],
                       fill=alpha(color[max(meta$wrap.group,na.rm=TRUE)+1-i],meta$alpha),
                       stroke=alpha(color[max(meta$wrap.group,na.rm=TRUE)+1-i],meta$alpha))
          }
        }
      }
    }
  }
  
  main_line_draw <- function(layer,painter){
    for (j in 1:meta$nyvar) {
      color=gray(seq(0,0.6,length=max(meta$wrap.group,na.rm=TRUE)))
      for (k in unique(meta$vargroup)) {
        for (i in 1:max(meta$wrap.group,na.rm=TRUE)) {
          if (sum(meta$wrap.group==i & meta$vargroup==k)){
            tmprow <- 1:meta$singleVarLen + meta$singleVarLen * (j-1)
            qdrawLine(painter,
                      meta$xtmp[meta$wrap.group==i & meta$vargroup==k & 1:length(meta$xtmp) %in% tmprow],
                      meta$ytmp[meta$wrap.group==i & meta$vargroup==k & 1:length(meta$ytmp) %in% tmprow],
                      stroke=alpha(color[max(meta$wrap.group,na.rm=TRUE)+1-i],meta$alpha))
          }
        }
      }
    }
  }
  
  brush_draw <- function(layer, painter) {
    
    if (any(is.na(meta$pos))) return()
    hits <- selected(data)[meta$orderEnter]
    if (!any(hits)) return()
    
    if (meta$serie.mode) {
      xpos <- meta$start[1]
      ypos <- meta$start[2]      
      meta$xtmp[hits] <- meta$xtmp[hits] + meta$pos[1] - meta$start[1]
      selected_draw(meta,b,hits,painter)
      return()
    }
    
    if (meta$wrapF_dragT) {     
      qupdate(main_circle_layer)
      qupdate(main_line_layer)
      return()
    }
    
    selected_draw(meta,b,hits,painter)
    draw_brush(layer, painter, data, meta)
  }
  
  query_draw <- function(item, painter, exposed, ...) {
    if (is.null(meta$query.pos)) return()      
    xpos <- meta$query.pos[1]
    ypos <- meta$query.pos[2]
    
    if (!meta$serie.mode){
      queryaround <- ifelse(meta$radius<=4,8/meta$radius,1)
      xrange <- meta$radius/layer.root$size$width() * diff(meta$limits[c(1, 2)]) * queryaround
      yrange <- meta$radius/layer.root$size$height() * diff(meta$limits[c(3, 4)]) * queryaround
  
      rect <- matrix(c(xpos - xrange, ypos - yrange, xpos + xrange, ypos + yrange),
                     2, byrow = TRUE)
      hits <- rectLookup(tree, rect[1, ], rect[2, ])
  
      ## Nothing under mouse?
      if (length(hits) < 1) return()
  
      if (length(hits) > 1) {
        hitsdist <- rep(0,length(hits))
        for (i in 1:length(hits)){
          hitsdist[i] <- sqrt((xpos-meta$xtmp[hits[i]])^2 + (ypos-meta$ytmp[hits[i]])^2)
        }
        distidx <- which(hitsdist==min(hitsdist,na.rm=TRUE))
        hits <- hits[distidx]
      }
      if (length(meta$group)==0) {
        info <- data.frame(meta$varname$x,meta$xtmp[hits],
                           meta$yorig[hits,1],meta$ytmp[hits])
      } else {
        info <- data.frame(meta$varname$x, meta$xtmp[hits],
                           meta$yorig[hits,1],meta$ytmp[hits],
                           meta$varname$g,meta$group[hits])
      }
  
      ## label position
      labelxpos <- mean(meta$xtmp[hits])
      labelypos <- mean(meta$ytmp[hits])
  
      ## label text 
      idx <- (1:(ncol(info)/2))*2
      if (length(hits) == 1) {
        infoname <- as.character(unlist(info[1,idx-1]))
        infodata <- as.character(unlist(info[1,idx]))
        infostring <- paste(infoname, infodata, collapse = "\n", sep = ": ")
      } else {
        xymin <- unlist(lapply(info[, idx], min, na.rm = TRUE))
        xymax <- unlist(lapply(info[, idx], max, na.rm = TRUE))
        if (max(table(info[,3]))==1){          
          infostring <- paste(as.character(unlist(info[1,idx-1])), paste(xymin, xymax, sep = " - "),
                              collapse = "\n", sep = ": ")
          infostring <- paste(length(hits),"points\n", infostring)
        } else {
          infoname <- c(as.character(unlist(info[1,idx-1])[-2]),unique(info[,3]))
          xymin <- c(xymin[-2],tapply(info[,4],info[,3],min))
          xymax <- c(xymax[-2],tapply(info[,4],info[,3],max))
          infodata <- paste(xymin, xymax, sep = " - ")
          infostring <- paste(infoname, infodata, collapse = "\n", sep = ": ")
        }
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
  
    } else {
      if (length(meta$group)==0 & meta$nyvar==1) return()
      xrange <- diff(meta$limits[c(1, 2)])/10
      yrange <- diff(meta$limits[c(3, 4)])/10        
      rect <- matrix(c(xpos - xrange, ypos - yrange, xpos + xrange, ypos + yrange),
                     2, byrow = TRUE)
      hits <- rectLookup(tree, rect[1, ], rect[2, ])
      if (length(hits) < 1) return()
      if (length(hits) > 1) {
        hitsdist <- rep(0,length(hits))
        for (i in 1:length(hits)){
          hitsdist[i] <- sqrt((xpos-meta$xtmp[hits[i]])^2 + (ypos-meta$ytmp[hits[i]])^2)
        }
        distidx <- which.min(hitsdist)
        hits <- hits[distidx]
      }
      if (length(meta$group)) {
        checkhitgroup <- meta$group==meta$group[hits] 
      } else {
        checkhitgroup <- rep(TRUE, length(meta$ytmp))
      }
      hitsall <- which(meta$yorig[,1]==meta$yorig[hits,1] & checkhitgroup)
      selected(data) <- hitsall[meta$orderBack]
      hits <- selected(data)[meta$orderEnter]
      selected_draw(meta,b,hits,painter)
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
  layer.grid = qgrid(meta = meta)
  main_circle_layer <- qlayer(paintFun = main_circle_draw,
                              limits = qrect(meta$limits),
                              hoverMoveFun = query_hover,
                              hoverLeaveFun = query_hover_leave,
                              mousePressFun = brush_mouse_press, 
                              mouseReleaseFun = brush_mouse_move,
                              mouseMove = brush_mouse_move,
                              wheelFun = mouse_wheel,
                              focusInFun = function(layer, event) {
                                common_focus_in(layer, event, data, meta)
                              }, focusOutFun = function(layer, event) {
                                common_focus_out(layer, event, data, meta)
                              }, clip=TRUE)
  main_line_layer <- qlayer(paintFun=main_line_draw,limits=qrect(meta$limits),clip=TRUE)
  brush_layer <- qlayer(paintFun=brush_draw, limits=qrect(meta$limits))
  query_layer <- qlayer(paintFun=query_draw, limits=qrect(meta$limits))
  
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
  view
  #return(meta)
}

data(nasa)
nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
#qnasa <- time_qdata(nasa11,"ts")
#qnasa <- time_qdata(nasa11,c("ts","ca_med"))
qnasa <- time_qdata(nasa11,c("ts","ps_tovs","ca_med"))
a=qtime2(TimeIndx,qnasa,shift=c(1,12))
a
