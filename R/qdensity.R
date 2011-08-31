#' Draw a univariate density plot
#'
#' Draw a univariate density plot, with a rug plot underneath.
#'
#' arrow up/down: in-/de-crease size of points
#'
#' arrow -/+: de-/in-crease alpha level (starts at alpha=1 by default)
#'
#' arrow left/right: de-/in-crease binwidth for density
#'
#' Key 'z' toggle zoom on/off (default is off): mouse click & drag will specify a zoom window, reset to default window by click/no drag
#'
#' Key 'x' toggle focal zoom on/off (default is off): mouse click & drag will specify a zoom window, zoom out by pressing shift key
#'
#' Key 'r' resets data range to original scale
#' @param x which designates variable displayed on the horizontal axis
#' @param data mutaframe data to use
#' @param size point size
#' @param alpha transparency level, 1=completely opaque
#' @param xlim = c(min, max) user specifed data range for the x axis, by default range(x)
#' @param ylim = c(min, max) user specifed data range for the y axis, by default range(y)
#' @param xlab label on horizontal axis, default is name of x variable
#' @param ylab label on vertical axis, default is name of y variable
#' @param cache boolean to turn cache on for layers, defaults to TRUE
#' @export
#' @example inst/examples/qdensity-ex.R
qdensity <- function(x, data, main = NULL, binwidth = NULL,
  size = 4, alpha = 0.5, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL,
  cache = T, ...)
{
  stopifnot(is.mutaframe(data))

  ################################
  # data processing & parameters #
  ################################

  arguments <- as.list(match.call()[-1])

  ## extract the variable of interest from data
  df <- data.frame(data)
  x <- eval(arguments$x, df)

  stopifnot(!is.null(x))
  stopifnot(length(x) > 1)

  # Initialize plot limits
  if (is.null(xlim))
    xlim <- range(x, na.rm=T)
  if (is.null(ylim))
    ylim <- c(0, max(density(x, bw=(xlim[2]-xlim[1])/75)$y))

  # Initalize binwidth to default for the density function
  if (is.null(binwidth))
    binwidth <- density(x)$bw
  
  ## parameters for dataRanges
  if (is.null(xlab)) xlab <- deparse(arguments$x)
  ylab <- ""

  values_out_of_plotting_range <- FALSE

  ## parameters for all layers
  dataRanges <- c(extend_ranges(xlim), extend_ranges(ylim, 0.15))

  lims <- qrect(dataRanges[c(1, 2)], dataRanges[c(3, 4)])

  ## parameters for datalayer
  .radius <- size
  .alpha <- alpha

  ## parameters used in event handling
  .brush <- TRUE
  ## mouse position
  .bpos <- c(NA, NA)
  ## drag start
  .bstart <- c(mean(dataRanges[c(1, 2)]), 0)
  ## move brush?
  .bmove <- TRUE
  ## brush range: horizontal and vertical
  .bsize <- c(diff(dataRanges[c(1, 2)])/15, diff(dataRanges[c(3, 4)])/30)
  .bsizestart <- .bsize # For brush resizing
  
  n <- nrow(data)

  # For accessing the global brush parameters
  b = brush(data)  # the brush attached to the data

  ## zooming on?
  zoom <- FALSE
  .zstart <- NULL
  .zstop <- NULL

  zoom_focal <- FALSE
  ################################ helper function

  updatelimits <- function(xlim, ylim) {
    dataRanges <<- c(xlim, ylim)

    xaxis$setLimits(qrect(dataRanges[1:2], c(0, 1)))
    yaxis$setLimits(qrect(c(0, 1), dataRanges[3:4]))

    lims <<- qrect(dataRanges[1:2], dataRanges[3:4])

    bglayer$setLimits(lims)
    datalayer$setLimits(lims)
    brushlayer$setLimits(lims)
    querylayer$setLimits(lims)

    qupdate(root)
  }

  ################################ end data processing & parameters

  ##########
  # layers #
  ##########

  xaxis <- function(item, painter, exposed) {
    sx <- axis_loc(dataRanges[1:2])

    draw_x_axes_with_labels_fun(painter, c(dataRanges[1:2],1,5),
      axisLabel = sx, labelHoriPos = sx, name = xlab)
  }

  yaxis <- function(item, painter, exposed) {
    sy <- axis_loc(dataRanges[3:4])

    draw_y_axes_with_labels_fun(painter, c(1,5, dataRanges[3:4]),
      axisLabel = sy, labelVertPos = sy, name = ylab)
  }

  grid <- function(item, painter, exposed) {
    sx <- axis_loc(dataRanges[1:2])
    sy <- axis_loc(dataRanges[3:4])

    # grey background with grid lines
    draw_grid_with_positions_fun(painter, dataRanges, sx, sy)
  }

  draw_points_density <- function(item, painter, exposed) {
    # Set drawing attributes
    fill <- data$.color
    stroke <- data$.color
    # Get data column
    df <- data.frame(data)
    x <- eval(arguments$x, df)
    y <- rep(0, length(x))
    # Compute density function, and draw
    ncol <- unique(stroke)
    for (i in 1:length(ncol)) {
      sc <- ncol[i]
      dx <- density(x[data$.color == sc], bw=binwidth)
      qlineWidth(painter) <- 3
      qdrawLine(painter, x=dx$x, y=dx$y, stroke = alpha(sc, 1))
    }

    # Draw points
    radius <- .radius
    qdrawCircle(painter, x = x, y = y, r = radius, fill = alpha(fill,
      .alpha), stroke = alpha(stroke, .alpha))

    # Draw indications that there is data outside plot window
    if (values_out_of_plotting_range) {
      if (any(x < dataRanges[1]))
        qdrawSegment(painter, x0 = dataRanges[1], x1=dataRanges[1],
          y0=dataRanges[3], y1=dataRanges[4], stroke = "red")
      if (any(x > dataRanges[2]))
        qdrawSegment(painter, x0 = dataRanges[2], x1=dataRanges[2],
          y0=dataRanges[3], y1=dataRanges[4], stroke = "red")
      if (any(y < dataRanges[3]))
        qdrawSegment(painter, x0 = dataRanges[1], x1=dataRanges[2],
          y0=dataRanges[3], y1=dataRanges[3], stroke = "red")
      if (any(y > dataRanges[4]))
        qdrawSegment(painter, x0 = dataRanges[1], x1=dataRanges[2],
          y0=dataRanges[4], y1=dataRanges[4], stroke = "red")
    }
  }

  brush_draw <- function(item, painter, exposed) {
    df <- as.data.frame(data)
    .brushed <- data$.brushed
    # Check if in brush mode, and do brushing if so
    if (.brush) {
      if (!any(is.na(.bpos))) {
        qlineWidth(painter) <- b$style$linewidth
        ##qdash(painter)=c(1,3,1,3)
        qdrawRect(painter, .bpos[1] - .bsize[1], .bpos[2] - .bsize[2],
          .bpos[1], .bpos[2] + .bsize[2],
          stroke <- brush(data)$color)
          qlineWidth(painter) <- 2*brush(data)$size
          qdrawSegment(painter, .bpos[1], .bpos[2] - .bsize[2],
                 .bpos[1], .bpos[2] + .bsize[2],
                 stroke = brush(data)$color)
      }

      hdata <- subset(df, .brushed)

      if (nrow(hdata) > 0) {
        ## draw the brush rectangle
        if (!any(is.na(.bpos))) {
        qlineWidth(painter) <- b$style$linewidth
          ##qdash(painter)=c(1,3,1,3)
          qdrawRect(painter, .bpos[1] - .bsize[1],
            .bpos[2] - .bsize[2], .bpos[1],
            .bpos[2] + .bsize[2], stroke = b$color)
          qlineWidth(painter) = 2*brush(data)$size
          qdrawSegment(painter, .bpos[1], .bpos[2] - .bsize[2],
                 .bpos[1], .bpos[2] + .bsize[2],
                 stroke = b$color)
        }

        # Compute density function, and draw
        stroke <- brush(data)$color
        qlineWidth(painter) <- 2*brush(data)$size
        dx <- density(eval(arguments$x, hdata), bw=binwidth)
        qlineWidth(painter) <- 3
        qdrawLine(painter, x=dx$x, y=dx$y, stroke = alpha(stroke, 1))

        ## (re)draw brushed data points
        brushx <- eval(arguments$x, hdata)
        brushy <- rep(0, length(brushx))
        fill <- b$color
        stroke <- b$color
        radius <- .radius*2

        qdrawCircle(painter, x = brushx, y = brushy, r = radius,
          fill = fill, stroke = stroke)
      }
    }
  }
  ########## end layers

  ####################
  ## event handlers ##
  ####################

  keyPressFun <- function(item, event, ...) {
    # arrow up/down: in/de-crease point size
    # arrow left/right: de/in-crease alpha level
    # z toggle zoom on/off (default is off): mouse click & drag
    #   will specify a zoom window
    key <- event$key()

    if (length(i <- which(key == c(Qt$Qt$Key_Up, Qt$Qt$Key_Down)))) {
      # arrow up/down - point size
      .radius <<- max(0, .radius + c(1, -1)[i])
      qupdate(datalayer)
      qupdate(brushlayer)
    }
    else if (length(i <- which(key == c(c(Qt$Qt$Key_Plus, Qt$Qt$Key_Minus))))) {
      # +/- - alpha blending
      .alpha <<- max(0.01, min(1, c(1.1, 0.9)[i] * .alpha))
      qupdate(datalayer)
    }
    else if (length(i <- which(key == c(Qt$Qt$Key_Right, Qt$Qt$Key_Left)))) {
      # arrow left/right - alpha blending
      binwidth <<- max((xlim[2]-xlim[1])/100, min((xlim[2]-xlim[1])/3,
        c(1.1, 0.9)[i] * binwidth))
      qupdate(datalayer)
    }
     else if (key == Qt$Qt$Key_Z) {
      zoom <<- !zoom
      print(sprintf("zoom: %s", zoom))
    }
    else if (key == Qt$Qt$Key_X) {
      zoom_focal <<- !zoom_focal
    }
    #else if (key == Qt$Qt$Key_R) {
    # reset to original boundaries
    #  updatelimits(extend_ranges(xlim), extend_ranges(ylim))
    #}

  }
  
  handle_zoom <- function() {
    print("zoom action")
    print(.zstart)
    print(.zstop)
    if (!all(.zstart == .zstop))
      updatelimits(c(min(.zstart[1],.zstop[1]), max(.zstart[1],
         .zstop[1])),c(min(.zstart[2],.zstop[2]),
         max(.zstart[2],.zstop[2])))
    else # reset zoom to default
      updatelimits(extend_ranges(xlim), extend_ranges(ylim))
    .zstart <<- .zstop <<- NULL
  }

  handle_focal_zoom <- function(focus, speed,  forward) {
    xscale <- (dataRanges[2]-focus[1])/(focus[1] - dataRanges[1])
    yscale <- (dataRanges[4]-focus[2])/(focus[2] - dataRanges[3])
      sgn <- if (forward) 1 else -1

    newRanges <- dataRanges
    newRanges[1] <- dataRanges[1] + sgn* speed*diff(dataRanges[1:2])
    newRanges[2] <- focus[1] + (focus[1]-newRanges[1]) * xscale
    newRanges[3] <- dataRanges[3] + sgn*speed*diff(dataRanges[3:4])
    newRanges[4] <- focus[2] + (focus[2]-newRanges[3]) * yscale

    df <- data.frame(data)
    sub <- subset(df, (x >= newRanges[1]) & (x <= newRanges[2]) &
      (y >= newRanges[3]) & (y <= newRanges[4])   )

    values_out_of_plotting_range <<- nrow(sub) < nrow(df)
    if (nrow(sub) >= 2) {
      updatelimits(newRanges[1:2], newRanges[1:2])
    }
  }

  ## record the coordinates of the mouse on click
  brush_mouse_press <- function(item, event) {
    if (zoom_focal) {
      handle_focal_zoom(as.numeric(event$pos()), 2,
        event$modifiers() != Qt$Qt$ShiftModifier )
    }
    else {
      if (zoom) {
        .zstart <<- as.numeric(event$pos())
      }
      else {
        .bstart <<- as.numeric(event$pos()) # brush jumps to mouse position
        .bsizestart <<- .bsize # brush jumps to mouse position
        .bstart[2] <<- 0
         ## on right click, we can resize the brush; left click: only move the
         ## brush
        if (event$button() == Qt$Qt$RightButton) {
          .bmove <<- FALSE
        }
        if (event$button() == Qt$Qt$LeftButton) {
          .bmove <<- TRUE
        }
      }
    }
  }

  mouse_release <- function(item, event, ...) {

    .bpos <- as.numeric(event$pos())
    if (zoom) {
      .zstop <<- .bpos
      handle_zoom()
    }
  }

  mouse_move <- function(layer, event) {
    if (zoom) {
      .zstop <<- as.numeric(event$pos())
      qupdate(querylayer)
      return()
    }
    pos <- event$pos()
    .bpos <<- as.numeric(pos)
    .bpos[2] <<- 0
    ## simple click: don't change .bsize
    ## Resize the brush
    if (!all(.bpos == .bstart) && (!.bmove)) {
      # Current size, plus difference in mouse move position
      .bsize[1] <<- .bsizestart[1] +  (.bpos[1] - .bstart[1])
      rect <- qrect(matrix(c(.bstart[1] - .bsize[1], -.bsize[2],
        .bstart[1] + (.bpos[1] - .bstart[1]), .bsize[2]), 2, byrow = TRUE))
    }
    else { # Brushing
      rect <- qrect(matrix(c(.bstart[1] - .bsize[1], -.bsize[2],
        .bstart[1] + (.bpos[1] - .bstart[1]), .bsize[2]), 2, byrow = TRUE))
    }
    .new.brushed <- rep(FALSE, n)

    # Do my own detection!!
    df <- as.data.frame(data)
    x <- eval(arguments$x, df)
    hits <- c(1:nrow(df))[x > min((.bpos[1] - .bsize[1]), .bpos[1]) &
      x < max((.bpos[1] - .bsize[1]), .bpos[1])]

    .new.brushed[hits] <- TRUE
    data$.brushed <- mode_selection(data$.brushed, .new.brushed,
      mode = b$mode)
    self_link(data)
  }

  handle_wheel_event <- function(layer, event) {
    handle_focal_zoom(as.numeric(event$pos()), event$delta()/200.0,  TRUE)
  }

  # Display category information on hover (query) ----------------------------
  .queryPos <- NULL

  query_draw <- function(item, painter, exposed, ...) {
    if (zoom) {
      if (!is.null(.zstart)) {
        if (!all(.zstart == .zstop)) {
         # draw zoom rectangle
          rect <- qrect(min(.zstart[1],.zstop[1]),
            max(.zstart[1],.zstop[1]), min(.zstart[2],.zstop[2]),
            max(.zstart[2],.zstop[2]))
          qdrawRect(painter, min(.zstart[1],.zstop[1]),
            min(.zstart[2],.zstop[2]), max(.zstart[1],.zstop[1]),
            max(.zstart[2],.zstop[2]), stroke="black",
            fill=rgb(.1,.1,.1, alpha=0.5))
          return()
        }
      }
    }

    if (is.null(.queryPos))
      return()

    xpos <- .queryPos[1]
    ypos <- 0

    xrange <- diff(dataRanges[c(1, 2)])/100
    yrange <- diff(dataRanges[c(3, 4)])/1000
    rect <- qrect(matrix(c(xpos - xrange, ypos - yrange,
      xpos + xrange, ypos + yrange), 2, byrow = TRUE))

    # My own identification function, finds the closest point
    df <- as.data.frame(data)
    x <- eval(arguments$x, df)
    dx <- sqrt((x-xpos)^2)
    if (min(dx) < diff(dataRanges[c(1, 2)])/100)
      hits <- order(dx, decreasing=F)[1]
    else
      hits <- NULL
     
    # Nothing under mouse?
    if (length(hits) == 0)
      return()

    info <- data.frame(x[hits])
    names(info) <- xlab

    # Nothing under mouse
    if (nrow(info) == 0) return()

    d <- options()$str$digits.d
    # Work out label text
    idx <- names(info)
    if (length(hits) == 1) {
      infodata <- as.character(round(unlist(info[, idx]), d))
      infostring <- paste(idx, infodata, collapse = "\n", sep = ": ")
    }
    else {
      xymin <- unlist(lapply(info[, idx], min, na.rm = TRUE))
      xymax <- unlist(lapply(info[, idx], max, na.rm = TRUE))
      xymin <- round(xymin, d)
      xymax <- round(xymax, d)
      infostring <- paste(idx, paste(xymin, xymax, sep = " - "),
        collapse = "\n", sep = ": ")
      infostring <- paste(sprintf("%s points\n", length(hits)), infostring)
    }
    infostring <- paste("\n", infostring)
    bgwidth <- qstrWidth(painter, infostring)
    bgheight <- qstrHeight(painter, infostring)

    ## adjust drawing directions when close to the boundary
    hflag <- dataRanges[2] - xpos > bgwidth
    vflag <- ypos - dataRanges[3] > bgheight
    qdrawRect(painter, xpos, ypos, xpos + ifelse(hflag, 1, -1) * bgwidth,
      ypos + ifelse(vflag, -1, 1) * bgheight,
      stroke = rgb(1, 1, 1), fill = rgb(1, 1, 1, 0.9))

    qstrokeColor(painter) <- b$label.color
    qdrawText(painter, infostring, xpos, ypos,
      halign = ifelse(hflag, "left", "right"),
      valign = ifelse(vflag, "top", "bottom"))
  }

  query_hover <- function(item, event, ...) {
    .queryPos <<- as.numeric(event$pos())
    qupdate(querylayer)
  }

  query_hover_leave <- function(item, event, ...) {
    .queryPos <<- NULL
    qupdate(querylayer)
  }

  ########## end event handlers

  #######################################################
  # scales
  scales_draw <- function(item, painter, exposed, ...) {
    # draw anchor point and line for bin width adjustment under first bin

    # anchor:
    eps <- diff(ylim)/50 # height of strokes
    qlineWidth(painter) <- 3
    qdrawSegment(painter, xlim[1], ylim[1]-4*eps, xlim[1], ylim[1]-5*eps,
    "grey50")
    # bin width adjust:
    qdrawSegment(painter, xlim[1]+binwidth, ylim[1]-4*eps,
    xlim[1]+binwidth, ylim[1]-5*eps, "grey50")
  }

  scales_handle_event <- function(pos) {
    xpos <- pos[1]
    ypos <- ylim[1]
    xeps <- diff(xlim)/125
    yeps <- diff(ylim)/50

    rect = qrect(matrix(c(xpos-xeps, ypos-4*yeps, xpos + xeps,
    ypos - 5*yeps), 2, byrow = TRUE))
    return(scaleslayer$locate(rect) + 1)
  }

  scales_hover <- function(item, event, ...) {
    pos <- as.numeric(event$pos())
    if (pos[2] < (-4*diff(ylim)/50)) {
      hits <- scales_handle_event(pos)

      if (length(hits) > 0) {
        hits <- hits[1]
        cu <- view$cursor
        switch(hits, {cursor <- Qt$Qt$SizeHorCursor},
        {cursor <- Qt$Qt$SizeHorCursor},
        {cursor <- Qt$Qt$UpArrowCursor}
        )
        cu$setShape(cursor)
        view$cursor <- cu
      }
    }
    else {
      cu <- view$cursor
      cu$setShape(Qt$Qt$ArrowCursor)
      view$cursor <- cu
    }
  }

  scales_mouse_press <- function(item, event, ...) {
    pos <- as.numeric(event$pos())
    if (pos[2] < (-4*diff(ylim)/50)) {
      # Only if the cursor is in the bottom part of the plot - scale area
      hits <- scales_handle_event(pos)
    }
    else
      brush_mouse_press(item, event, ...)
  }

  scales_mouse_release <- function(item, event, ...) {
    pos <- as.numeric(event$pos())
    if (pos[2] < (-4*diff(ylim)/50)) {
      # Only if the cursor is in the bottom part of the plot - scale area
      hits <- scales_handle_event(pos)

      if (length(hits) == 0) {
      # pass mouse event on
        mouse_release(item, event, ...)
      }
    }
  }

  scales_mouse_move <- function(item, event, ...) {
    pos <- as.numeric(event$pos())
    if (pos[2] < (-4*diff(ylim)/50)) {
      # Only if the cursor is in the bottom part of the plot - scale area
      hits <- scales_handle_event(pos)

      if (length(hits) > 0) {
        # change binwidth
        bwtmp <- pos[1] - xlim[1]
        if ((bwtmp > (xlim[2]-xlim[1])/100) & (bwtmp < (xlim[2]-xlim[1])/3))
          binwidth <<- pos[1] - xlim[1]

        scaleslayer$invalidateIndex()
        qupdate(scene)
      }
    }
    else {
      # pass mouse event on
      mouse_move(item, event, ...)
    }
  }

  ###################
  # draw the canvas #
  ###################
  xWidth <- 600
  yWidth <- 600

  scene <- qscene()
  root <- qlayer(scene, cache=cache)

  xaxis <- qlayer(parent=root, paintFun = xaxis,
     limits = qrect(dataRanges[1:2], c(0, 1)),
     cache=cache, row=2, col=1, clip=FALSE)
  yaxis <- qlayer(parent=root, paintFun = yaxis,
     limits = qrect(c(0, 1), dataRanges[3:4]),
     cache=cache, row=1, col=0, clip=FALSE)

# clipping needs to be turned off - otherwise small integer values cause strange results
  bglayer <- qlayer(parent = root, paintFun = grid,
    limits = lims, cache=cache, row=1, col=1, clip=FALSE)

  datalayer <- qlayer(parent = root, paintFun = draw_points_density,
    keyPressFun = keyPressFun, mouseMoveFun = mouse_move,
    mousePressFun = brush_mouse_press,
    mouseReleaseFun = mouse_move, #mouse_release - doesn't work,
    wheelFun = handle_wheel_event, limits = lims,
      cache=cache, row=1, col=1, clip=FALSE)

  brushlayer <- qlayer(parent = root, paintFun = brush_draw, limits = lims,
    cache=cache, row=1, col=1, clip=FALSE)
  
  querylayer <- qlayer(parent = root, query_draw, limits = lims,
     hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave,
     cache=cache, row=1, col=1, clip=FALSE)

  scaleslayer <- qlayer(root, scales_draw, limits = lims, clip = FALSE,
    mousePressFun = scales_mouse_press,
    mouseReleaseFun = scales_mouse_release,
    mouseMoveFun = scales_mouse_move,
    hoverMoveFun = scales_hover, row=1, col=1)

  layout = root$gridLayout()
  layout$setRowPreferredHeight(0, 40)  # width of yaxis
  ## the y-axis layer needs 'dynamic' width determined by #{characters}
  ## here is a formula by my rule of thumb: 9 * nchar + 5
  layout$setColumnPreferredWidth(0, 50)
  layout$setRowPreferredHeight(2, 50)
  layout$setColumnMaximumWidth(2, 10)
  layout$setRowStretchFactor(0, 0)
  layout$setColumnStretchFactor(0, 0)
  layout$setRowStretchFactor(2, 0)

 # space on right side
  layout$setColumnPreferredWidth(3,40) # height of xaxis
  layout$setColumnStretchFactor(3, 0)

  view <- qplotView(scene = scene)

  title <- sprintf("Density plot of %s", xlab)
  view$setWindowTitle(title)

  ######################
  # add some listeners #
  ######################

  func <- function(i, j) {
    switch(j, .brushed = qupdate(brushlayer),
      .color = qupdate(datalayer),
      {  # any other event
        datalayer$invalidateIndex()
        qupdate(datalayer)
        qupdate(brushlayer)
      })
  }

  add_listener(data, func)
  view$resize(xWidth, yWidth)

  return(view)
}
