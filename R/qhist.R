#' Create a hist plot
#'
#' Create a hist plot from 1-D numeric data
#'
#' keystroke Interactions:
#' right/left arrow shift anchors;
#' up/down arrow increase/decrease bin size;
#' M re-adjusts the maximum on the y-axis to 10% above the bin count
#' @param data mutaframe to use
#' @param x variable to plot
#' @param horizontal boolean to decide if the bars are horizontal or vertical
#' @param xlim = c(min, max) user specifed data range for the x axis, by default range(x)
#' @param ylim = c(min, max) user specifed data range for the y axis, by default range(y)
#' @param ... arguments supplied to hist() or the hist layer
#' @author Barret Schloerke, Di Cook, Heike Hofmann
#' @keywords hplot
#' @example inst/examples/qhist-ex.R
qhist <- function(x, data, splitByCol = -1, horizontal = FALSE,
  position = "none",
  main = NULL, name = NULL, ash = FALSE, start = NULL,
  nbins = round(sqrt(nrow(data)), 0), binwidth = NULL,
  bin_algo_str = NULL, xlim=NULL, ylim=NULL, cache=TRUE, ...) {

  stopifnot(is.mutaframe(data))

  arguments <- as.list(match.call()[-1])
  df <- data.frame(data)
  x <- eval(arguments$x, df)

  # 'Global' variables (start with a '.')
  .view <- c()
  .scene <- c()
  .type <- c()
  .data_col_names <- c()
  .startBrush <- NULL
  .endBrush <- NULL
  .brush <- FALSE
  .bar_queryPos <- NULL
  .bar_hover_section <- list(top = -1, bottom = 1, right = -1, left = 1)
  .lims <- c() # Window limits??
  .bars_info <- NULL
  if (is.null(start))
    start <- min(x)
  if (is.null(ylim)) {
    .yMin <- 0
    .yMax <- nrow(data)/2
  }
  else {
    .yMin <- ylim[1]
    .yMax <- ylim[2]
  }
  if (is.null(xlim))
    .dataranges <- c()  # Data limits?
  else
    .dataranges <- c(xlim, 0, .yMax)
  .xlab <- ""
  .ylab <- ""
  .histOriginalBreaksAndStart <- list()
  .updateinfo <- FALSE
  if (is.null(name)) {
    name <- as.character(arguments$x)
  }

  # Set up the data
  if (splitByCol == -1) {
    splitByCol <- "qhist_split_column"
    data[[splitByCol]] <- 1
  }
  .data_col_names <- rep("", nrow(data))

  xColRange <- function() {
    range(x)
  }
  maxBinwidthP <- function() {
    maxBinwidth(x)
  }
  baseHistP <- function(...) {
    baseHist(x, ...)
  }
  unitShiftP <- function() {
    unitShift(x)
  }
  maxShiftP <- function() {
    maxShift(x)
  }
  xMaxStartPosP <- function() {
    xMaxStartPos(x)
  }
  xMinStartPosP <- function() {
    xMinStartPos(x)
  }
  xMaxEndPosP <- function() {
    xMaxEndPos(x)
  }
  calcBinPosP <- function(start, binwidth) {
    calcBinPosition(start, binwidth, xColRange()[2], xMaxEndPosP())
  }
  maxHeightP <- function() {
    maxHeight(x, ...)
  }

  if (!is.null(bin_algo_str)) {
    temp_breaks <- baseHistP(breaks = bin_algo_str)$breaks
    .type <- list(type = "hist", binwidth = diff(temp_breaks[1:2]),
       start = temp_breaks[1])
  }
  else if (!is.null(binwidth)) {
    .type <- list(type = "hist", binwidth = binwidth, start = start)
  }
  else {
    if (nbins < 1) {
      stop("please supply a correct bin count")
    }
    temp_breaks <- baseHistP(breaks = nbins)$breaks
    .type <- list(type = "hist", binwidth = diff(xColRange())/nbins,
       start = start)
  }

  .histOriginalBreaksAndStart <- list(binwidth = .type$binwidth,
     start = .type$start)

  updateBarsInfo <- function() {
    .bars_info <<- continuous_to_bars(data = x,
      splitBy = data[, splitByCol], brushed = data[, ".brushed"],
      typeInfo = .type, position = position, ...)

    .data_col_names <<- rep("", length(.data_col_names))
    for (i in 1:nrow(.bars_info$data)) {
      rows <- (.bars_info$data$left[i] < x) &
        (.bars_info$data$right[i] >= x)
      if (any(rows)) {
        .data_col_names[rows] <<- as.character(.bars_info$data[i, "label"])
      }
    }
    .updateinfo <<- FALSE
  }
  updateBarsInfo()

  .yMax <- 1.1 * max(.bars_info$data$top)

  updateRanges <- function() {
    # contains c(x_min, x_max, y_min, y_max)
    if (horizontal) {
      if (is.null(xlim))
        .dataranges <<- c(extend_ranges(c(.yMin, .yMax)),
          extend_ranges(c(xMinStartPosP(), xMaxEndPosP())))
      else
        .dataranges <<- c(extend_ranges(c(.yMin, .yMax)), xlim)
    }
    else {
      if (is.null(xlim))
        .dataranges <<- c(extend_ranges(c(xMinStartPosP(),
          xMaxEndPosP())), extend_ranges(c(.yMin, .yMax)))
      else
        .dataranges <<- c(xlim, extend_ranges(c(.yMin, .yMax)))
    }
  }
  updateRanges()

  #######################################################
  # Draw Axes

  xaxis <- function(item, painter, exposed) {
    horiPos <- axis_loc(.dataranges[1:2])
    .xlab <- name
    if (horizontal) .xlab <- "count"

    draw_x_axes_with_labels_fun(painter, c(.dataranges[1:2],1,5), horiPos,
     horiPos, .xlab)
  }

  yaxis <- function(item, painter, exposed) {
    .ylab <- "count"
    if (horizontal) .ylab <- name

    vertPos <- axis_loc(.dataranges[3:4])
    draw_y_axes_with_labels_fun(painter, c(1,5,.dataranges[3:4]), vertPos,
      vertPos, .ylab)
  }

  grid <- function(item, painter, exposed) {
    sx <- axis_loc(.dataranges[1:2])
    sy <- axis_loc(.dataranges[3:4])

    # grey background with grid lines
    draw_grid_with_positions_fun(painter, .dataranges, sx, sy)
  }

  coords <- function(item, painter, exposed) {
    if (horizontal) {
      .ylab <<- name
      .xlab <<- "count"
    }
    else {
      .ylab <<- "count"
      .xlab <<- name
    }

    # grey background with grid lines
    horiPos <- axis_loc(.dataranges[1:2])
    vertPos <- axis_loc(.dataranges[3:4])

    draw_grid_with_positions_fun(painter, .dataranges, horiPos, vertPos)

    # put labels
    draw_x_axes_with_labels_fun(painter, .dataranges, horiPos, horiPos, .xlab)
    draw_y_axes_with_labels_fun(painter, .dataranges, vertPos, vertPos, .ylab)
    # title
    if (is.null(main)) main <- paste("Histogram of", name)
    add_title_fun(painter, .dataranges, main)
  }

  #######################################################
  # Draw Bars
  hist.all <- function(item, painter, exposed) {
    if (horizontal) {
      qdrawRect(painter, xleft = c(.bars_info$data$bottom),
        ybottom = c(.bars_info$data$left),
      xright = c(.bars_info$data$top), ytop = c(.bars_info$data$right),
      stroke = c(.bars_info$data$stroke), fill = c(.bars_info$data$fill))
    }
    else {
      qdrawRect(painter, xleft = c(.bars_info$data$left),
        ybottom = c(.bars_info$data$bottom),
      xright = c(.bars_info$data$right), ytop = c(.bars_info$data$top),
      stroke = c(.bars_info$data$stroke), fill = c(.bars_info$data$fill))
    }
  }


  #######################################################
  # Key Functions
  keyPressFun <- function(item, event, ...) {
    if (.brush == TRUE)
      return()

    key <- event$key()

    if (key == Qt$Qt$Key_Up) {
      .type$binwidth <<- min(.type$binwidth * 1.1, maxBinwidthP())
      updateBarsInfo()

      if (.yMax < max(.bars_info$data$top))
        .yMax <<- 1.1 * max(.bars_info$data$top)
      updateRanges()
      updateLims()
      scaleslayer$invalidateIndex()
      qupdate(.scene)
    }
    else if (key == Qt$Qt$Key_Down) {
      .type$binwidth <<- .type$binwidth/1.1
      updateBarsInfo()
      if (.yMax < max(.bars_info$data$top))
        .yMax <<- 1.1 * max(.bars_info$data$top)
      updateRanges()
      updateLims()
      scaleslayer$invalidateIndex()
      qupdate(.scene)
    }
    else if (key == Qt$Qt$Key_Left) {
      .type$start <<- .type$start - unitShiftP()
      # Make sure the start stays close to home
      if (.type$start < xMinStartPosP())
        .type$start <<- xMinStartPosP()
      updateBarsInfo()
      updateRanges()
      updateLims()
      scaleslayer$invalidateIndex()
      qupdate(.scene)
    }
    else if (key == Qt$Qt$Key_Right) {
      .type$start <<- .type$start + unitShiftP()
      # Make sure the start stays close to home
      if (.type$start > xMaxStartPosP())
        .type$start <<- xMaxStartPosP()
      updateBarsInfo()
      updateRanges()
      updateLims()
      scaleslayer$invalidateIndex()
      qupdate(.scene)
    }
    else if (key == Qt$Qt$Key_A) {
      .type$type <<- "ash"
      stop("Ash not implemented")
    }
    else if (key == Qt$Qt$Key_D) {
      .type$type <<- "density"
      updateBarsInfo()
      updateRanges()
      updateLims()
      qupdate(.scene)
      qupdate(bglayer)
      qupdate(datalayer)
      scaleslayer$invalidateIndex()
      qupdate(.scene)
    }
    else if (key == Qt$Qt$Key_O) {
      .type$type <<- "dot"
      stop("Ash not implemented")
    }
    else if (key == Qt$Qt$Key_H) {
      .type <- list(type = "hist",
         binwidth = .histOriginalBreaksAndStart$binwidth,
         start = .histOriginalBreaksAndStart$start)
    }
    else if (key == 82) {
      if (identical(.type$type, "hist")) {
        .type$type <<- "hist"
        .type$start <<- .histOriginalBreaksAndStart$start
        .type$binwidth <<- .histOriginalBreaksAndStart$binwidth
      }
    }
    else if (key == Qt$Qt$Key_M) {
      updateBarsInfo()
      .yMax <<- 1.1 * max(.bars_info$data$top)
      updateRanges()
      updateLims()
      scaleslayer$invalidateIndex()
      qupdate(.scene)
    }
    else if (key == 87) {
    }

  }
  #######################################################
  # Brushing
  draw_brush_rect <- function(item, painter, exposed) {
    left = min(.startBrush[1], .endBrush[1])
    right = max(.startBrush[1], .endBrush[1])
    top = max(.startBrush[2], .endBrush[2])
    bottom = min(.startBrush[2], .endBrush[2])

    qdrawRect(painter, left, bottom, right, top,
      fill = rgb(0, 0, 0, alpha = 0.7), stroke = "grey50")
  }

  brushing_draw <- function(item, painter, exposed, ...) {
    if (.updateinfo)
      updateBarsInfo()
    section <- subset(.bars_info$data, (.brushed > 0))

    if (nrow(section) > 0) {
      brushColor <- brush(data)$color

      b <- section$bottom
      t <- (section$top - b) * section$.brushed + b

      brushColor <- rep(brushColor, nrow(section))
      brushColorRGBA <- col2rgb(brushColor, TRUE)

      if (brushColorRGBA["alpha", ][[1]] > 200) {
        brushColorRGBA["alpha", ][[1]] <- 200
        newBrushColor <- rgb(brushColorRGBA["red", ][[1]],
          brushColorRGBA["green", ][[1]], brushColorRGBA["blue", ][[1]],
          brushColorRGBA["alpha", ][[1]], maxColorValue = 255)

        rows <- section$.brushed < 1
        brushColor[rows] <- newBrushColor
      }

      if (horizontal)
        qdrawRect(painter, b, section$right, t, section$left,
          fill = brushColor, stroke = "grey50")
      else
        qdrawRect(painter, section$left, b, section$right, t,
          fill = brushColor, stroke = "grey50")
    }

    if (!is.null(.endBrush)) {
      draw_brush_rect(item, painter, exposed)
    }
  }

  brushing_mouse_press <- function(item, event, ...) {
    .brush <<- TRUE
    if (is.null(.startBrush)) {
      .startBrush <<- as.numeric(event$pos())
    }
    .endBrush <<- as.numeric(event$pos())

    setHiliting()
    qupdate(brushing_layer)
  }

  brushing_mouse_move <- function(item, event, ...) {
    .endBrush <<- as.numeric(event$pos())

    setHiliting()
    qupdate(brushing_layer)
  }

  brushing_mouse_release <- function(item, event, ...) {
    .endBrush <<- as.numeric(event$pos())
    setHiliting()
    qupdate(brushing_layer)

    .brush <<- FALSE
    .startBrush <<- NULL
    .endBrush <<- NULL

    setSelected()
  }

  setHiliting <- function() {
    leftMouse = min(.startBrush[1], .endBrush[1])
    rightMouse = max(.startBrush[1], .endBrush[1])
    topMouse = max(.startBrush[2], .endBrush[2])
    bottomMouse = min(.startBrush[2], .endBrush[2])

    if (horizontal) {
      leftMouse = min(.startBrush[2], .endBrush[2])
      rightMouse = max(.startBrush[2], .endBrush[2])
      topMouse = max(.startBrush[1], .endBrush[1])
      bottomMouse = min(.startBrush[1], .endBrush[1])
    }

    valid_bar_row <<- function(original, left, right, top, bottom) {
      val <- (left <= rightMouse) && (right >= leftMouse) && (min(bottom) <=
        topMouse) && (max(top) >= bottomMouse)
      if (val) {
        1
      }
      else {
        0
      }
    }

    .bars_info$data <<- ddply(.bars_info$data, .(label), transform,
       .brushed = valid_bar_row(.brushed, left, right, top, bottom))
  }

  setSelected <- function() {
    section <- subset(.bars_info$data, .brushed == 1)

    rows <- rep(FALSE, nrow(data))
    if (NROW(section) > 0)
      rows <- .data_col_names %in% as.character(section$label)

    # update original data
    data$.brushed <- rows
  }


  #######################################################
  # Hover
  bar_hover_draw <- function(item, painter, exposed, ...) {
    # Don't draw when brushing
    if (is.null(.bar_queryPos))
      return()

    if (horizontal) {
      x <- .bar_queryPos[2]
      y <- .bar_queryPos[1]
    }
    else {
      x <- .bar_queryPos[1]
      y <- .bar_queryPos[2]
    }

    section <- subset(.bars_info$data, (y <= top) & (y >= bottom) &
      (x <= right) & (x >= left))

    # Nothing under mouse
    if (nrow(section) == 0) {
      .bar_hover_section <<- list(top = -1, bottom = 1, right = -1, left = 1)
      return()
    }

    # Highlight the rect
    brushColor <- brush(data)$color
    if (horizontal) {
      qdrawRect(painter, xleft = c(section$bottom), ybottom = c(section$right),
        xright = c(section$top), ytop = c(section$left), stroke = brushColor,
        fill = c(NA))
    }
    else {
      qdrawRect(painter, xleft = c(section$left),
        ybottom = c(section$bottom), xright = c(section$right),
          ytop = c(section$top), stroke = brushColor, fill = c(NA))
    }

    # Work out label text
    infostring <- paste("bin: ", section[1, "label"], sep = "")

    count <- section$top - section$bottom
    infostring <- paste(infostring, "\ncount: ", count, "/", nrow(data), sep = "")
    infostring <- paste(infostring, "\nproportion: ",
      pretty_percent(count/nrow(data)), sep = " ")
    if (splitByCol != "qhist_split_column") {
      infostring <- paste(infostring, "\ngroup: ", section[1, "group"], sep = "")
      colcount <- sum(.bars_info$data[.bars_info$data$label
        %in% section[1, "label"], "count"])
      val <- count/colcount
      #if (is.null(val)) {
        if (val != 1) {
          infostring <- paste(infostring, "\nbar: ",
            pretty_percent(val), " (", count, "/", colcount, ")", sep = "")
        }
    }

    xpos = .bar_queryPos[1]
    ypos = .bar_queryPos[2]

    bgwidth = qstrWidth(painter, infostring)
    bgheight = qstrHeight(painter, infostring)
    hflag = .dataranges[2] - xpos > bgwidth
    vflag = ypos - .dataranges[3] > bgheight

    qdrawRect(painter, xpos, ypos, xpos + ifelse(hflag, 1, -1) *
          bgwidth, ypos +
      ifelse(vflag, -1, 1) * bgheight, stroke = rgb(1, 1, 1), fill = rgb(1,
      1, 1, 0.9))

    qstrokeColor(painter) = brush(data)$label.color
    qdrawText(painter, infostring, xpos, ypos, halign = ifelse(hflag, "left",
      "right"), valign = ifelse(vflag, "top", "bottom"))

    .bar_hover_section <<- list(top = section$top, bottom = section$bottom,
      left = section$left, right = section$right)
  }

  bar_hover <- function(item, event, ...) {
    .bar_queryPos <<- as.numeric(event$pos())

    if (horizontal) {
      x <- .bar_queryPos[2]
      y <- .bar_queryPos[1]
    }
    else {
      x <- .bar_queryPos[1]
      y <- .bar_queryPos[2]
    }

    if (!((y <= .bar_hover_section$top) & (y >= .bar_hover_section$bottom) &
      (x <= .bar_hover_section$right) & (x >= .bar_hover_section$left))) {
      qupdate(hoverlayer)
    }
  }

  bar_leave <- function(item, event, ...) {
    qupdate(hoverlayer)
  }
  #######################################################
  # scales
  scales_draw <- function(item, painter, exposed, ...) {
    # draw anchor point and line for bin width adjustment under first bin

    # anchor:
    eps <- diff(.dataranges[3:4])/50
    qdrawSegment(painter, .bars_info$data$left[1],
      .bars_info$data$bottom[1]-eps/5, .bars_info$data$left[1],
      .bars_info$data$bottom[1]-eps, "black")
    # bin width adjust:
    qdrawSegment(painter, .bars_info$data$left[2],
      .bars_info$data$bottom[2]-eps/5, .bars_info$data$left[2],
      .bars_info$data$bottom[2]-eps, "black")

    # max bin height cue area:
    qdrawRect(painter, .dataranges[1], .dataranges[4]-diff(.dataranges[3:4])/10,
      .dataranges[2], .dataranges[4], stroke=NA, fill=NULL)
  }

  scales_handle_event <- function(pos) {
    xpos <- pos[1]
    ypos <- pos[2]
    xeps <- diff(.dataranges[1:2])/125
    yeps <- diff(.dataranges[3:4])/125

    rect = qrect(matrix(c(xpos-xeps, ypos-yeps, xpos + xeps, ypos + yeps),
      2, byrow = TRUE))
    return(scaleslayer$locate(rect) + 1)
  }

  scales_hover <- function(item, event, ...) {

    hits <- scales_handle_event(as.numeric(event$pos()))

    if (length(hits) > 0) {
      hits <- hits[1]
      cu <- .view$cursor
      switch(hits, {cursor <- Qt$Qt$SizeHorCursor}, {cursor <- Qt$Qt$SizeHorCursor},
        {cursor <- Qt$Qt$UpArrowCursor})
      cu$setShape(cursor)
      .view$cursor <- cu
    }
    else {
      cu <- .view$cursor
      cu$setShape(Qt$Qt$ArrowCursor)
      .view$cursor <- cu

      bar_hover(item, event, ...)
    }
  }

  scales_mouse_press <- function(item, event, ...) {
    hits <- scales_handle_event(as.numeric(event$pos()))

    if (length(hits) > 0) {
      updateBarsInfo()
      switch (hits, {}, {}, {.yMax <<- 1.1 * max(.bars_info$data$top)})
      updateRanges()
      updateLims()
      scaleslayer$invalidateIndex()
      qupdate(.scene)
    }
    else
      brushing_mouse_press(item, event, ...)  # pass mouse event on
  }

  scales_mouse_release <- function(item, event, ...) {
    hits <- scales_handle_event(as.numeric(event$pos()))

    if (length(hits) == 0) {
    # pass mouse event on
      brushing_mouse_release(item, event, ...)
    }
  }

  scales_mouse_move <- function(item, event, ...) {
    pos <- as.numeric(event$pos())
    hits <- scales_handle_event(pos)

    if (length(hits) > 0) {
      switch(hits, {
        # change anchor point
        .type$start <<- pos[1]
        }, {
        # change binwidth
       .type$binwidth <<- pos[1] - .bars_info$data$left[1]
      })
      updateBarsInfo()
      updateRanges()
      updateLims()
      scaleslayer$invalidateIndex()
      qupdate(.scene)
    }
    else {
      # pass mouse event on
      brushing_mouse_move(item, event, ...)
    }
  }

  #######################################################
  # Layout
  updateLims <- function(xlim=.dataranges[1:2], ylim=.dataranges[3:4]) {
    .dataranges <<- c(xlim, ylim)

    xaxis$setLimits(qrect(.dataranges[1:2], c(0, 1)))
    yaxis$setLimits(qrect(c(0, 1), .dataranges[3:4]))

    .lims <<- qrect(xlim, ylim)

    bglayer$setLimits(.lims)
    datalayer$setLimits(.lims)
    brushing_layer$setLimits(.lims)
    hoverlayer$setLimits(.lims)
    scaleslayer$setLimits(.lims)
  }

  .lims <- qrect(.dataranges[1:2], .dataranges[3:4])

  .scene <- qscene()
  root <- qlayer(.scene, cache=cache)

  xaxis <- qlayer(parent=root, paintFun = xaxis, limits =
    qrect(.dataranges[1:2], c(0, 1)), cache=cache, row=2, col=1, clip=FALSE)
  yaxis <- qlayer(parent=root, paintFun = yaxis, limits =
    qrect(c(0, 1), .dataranges[3:4]), cache=cache, row=1, col=0, clip=FALSE)

  bglayer <- qlayer(parent= root, paintFun = grid, limits = .lims,
    keyPressFun = keyPressFun, cache=cache, row=1, col=1, clip=FALSE)

  datalayer <- qlayer(root, hist.all, limits = .lims, clip = FALSE, row=1, col=1)
  brushing_layer <- qlayer(root, brushing_draw,
    mousePressFun = brushing_mouse_press,
    mouseMoveFun = brushing_mouse_move,
    mouseReleaseFun = brushing_mouse_release,
    limits = .lims, clip = FALSE, row=1, col=1)

  hoverlayer <- qlayer(root, bar_hover_draw, limits = .lims, clip = FALSE,
    hoverMoveFun = bar_hover, hoverLeaveFun = bar_leave, row=1, col=1)

  scaleslayer <- qlayer(root, scales_draw, limits = .lims, clip = FALSE,
    mousePressFun = scales_mouse_press,
    mouseReleaseFun = scales_mouse_release,
    mouseMoveFun = scales_mouse_move,
    hoverMoveFun = scales_hover, row=1, col=1)

  # update the brush layer in case of any modifications to the mutaframe
  func <- function(i, j=NULL) {
    switch (j, .brushed = {
      qupdate(brushing_layer)
      updateBarsInfo()},
      {
        updateBarsInfo()
        if (.yMax < max(.bars_info$data$top)) {
          .yMax <<- 1.1 * max(.bars_info$data$top)
        }
        updateRanges()
        updateLims()
        scaleslayer$invalidateIndex()
        qupdate(.scene)
    })
  }

  add_listener(data, func)

  brush_update = function() {
    qupdate(brushing_layer)
  }

  layout <- root$gridLayout()
  layout$setRowPreferredHeight(0, 40)  # white space above plot
  layout$setColumnPreferredWidth(0, 50) # width of yaxis
  layout$setRowPreferredHeight(2, 50) # height of xaxis
  layout$setColumnPreferredWidth(2, 40) # white space on right side
  layout$setRowStretchFactor(0, 0)
  layout$setColumnStretchFactor(0, 0)
  layout$setRowStretchFactor(2, 0)

  .view <- qplotView(scene = .scene)

  if (is.null(main))
    main <- sprintf("Histogram of %s", name)
  .view$setWindowTitle(main)

  .view
}
