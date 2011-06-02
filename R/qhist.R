#' Create a hist plot
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
#' @example cranvas/inst/examples/qhist-ex.R
qhist <- function(x, data, splitByCol = -1, horizontal = FALSE, 
    position = "none", 
    title = NULL, name = NULL, ash = FALSE, start = min(data[[x]]),
    nbins = round(sqrt(nrow(data)), 0), binwidth = NULL, 
    bin_algo_str = NULL, xlim=NULL, ylim=NULL, ...) {

    stopifnot(is.mutaframe(data))

    # 'Global' variables (start with a '.')
    .view <- c()
    .scene <- c()
    .type <- c()
    # .bin_col <- ''
    # .label_col <- ''
    .data_col_names <- c()
    .startBrush <- NULL
    .endBrush <- NULL
    .brush <- FALSE
    .bar_queryPos <- NULL
    .bar_hover_section <- list(top = -1, bottom = 1, right = -1, left = 1)
    .lims <- c() # Window limits??
    .bars_info <- NULL
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
    # message("limits 1 ",.dataranges[1], " ", .dataranges[2]," ", .dataranges[3], " ",.dataranges[4], "\n")
    .xlab <- ""
    .ylab <- ""
    .histOriginalBreaksAndStart <- list()
    .updateinfo <- FALSE
    if (is.null(name)) { 
        if (is.character(x))
            name <- x
        else
          name <- names(data)[x]
    }
    # mf_data <- qdata(data)
    #if (!is.mutaframe(data)) 
    #    mf_data <- qdata(data)
    #else
    #mf_data <- data
    
    # Set up the data
    if (splitByCol == -1) {
        splitByCol <- "qhist_split_column"
        data[[splitByCol]] <- 1
    }
    # .bin_col <- data_bin_column(mf_data)
    .data_col_names <- rep("", nrow(data))
    #print(head(data))

    # Check for fill colors
    #if (length(unique(data$.fill)) > 1)
    #  fill <- data$.fill
    # message("fill 0 ",length(fill),"\n")
       
    # Set up wrapper functions.
    dataCol <- function() {
        data[, x]
    }
    xColRange <- function() {
        range(dataCol())
    }
    maxBinwidthP <- function() {
        maxBinwidth(dataCol())
    }
    baseHistP <- function(...) {
        baseHist(dataCol(), ...)
    }
    unitShiftP <- function() {
        unitShift(dataCol())
    }
    maxShiftP <- function() {
        maxShift(dataCol())
    }
    xMaxStartPosP <- function() {
        xMaxStartPos(dataCol())
    }
    xMinStartPosP <- function() {
        xMinStartPos(dataCol())
    }
    xMaxEndPosP <- function() {
        xMaxEndPos(dataCol())
    }
    calcBinPosP <- function(start, binwidth) {
        calcBinPosition(start, binwidth, xColRange()[2], xMaxEndPosP())
    }
    maxHeightP <- function() {
        maxHeight(dataCol(), ...)
    }
    # cat('.xMin: ', xMinStartPosP(), '.xMax: ', xMaxEndPosP(), '\n')
    # cat('maxShift(): ', maxShiftP(), '\n')
    # cat('maxBinwidth(): ', maxBinwidthP(), '\n')
    
    
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

    .histOriginalBreaksAndStart <- list(binwidth = .type$binwidth, start = .type$start)
    # cat('\nOriginal Hist Breaks!\n'); print(.histOriginalBreaksAndStart);
    # cat('start: ');print(start)
    # cat('bin_algo_str: ');print(bin_algo_str)
    # cat('binwidth: ');print(binwidth)
    # cat('nbins: ');print(nbins)
    
    updateBarsInfo <- function() {
        .bars_info <<- continuous_to_bars(data = data[, x], splitBy = data[, 
            splitByCol], brushed = data[, ".brushed"], typeInfo = .type,
            position = position, ...)

        .data_col_names <<- rep("", length(.data_col_names))
        for (i in 1:nrow(.bars_info$data)) {
            rows <- (.bars_info$data$left[i] < dataCol()) &
              (.bars_info$data$right[i] >= dataCol())
            # cat(i, ' - '); print(rows)
            if (any(rows)) {
                .data_col_names[rows] <<- as.character(.bars_info$data[i, "label"])
            }
        }
#        cat("data: \n")
#        print(data[1:8, ])
#        cat("condensed data: \n")
#        print(.bars_info$data[, c("label", "group", "count", "top", "bottom", ".brushed")])
#        cat("unique columns: ")
#        print(unique(.data_col_names))
        
        .updateinfo <<- FALSE
    }
    updateBarsInfo()
    #if (.yMax < 1.1 * max(.bars_info$data$count))
    .yMax <- 1.1 * max(.bars_info$data$top)
    #message(.yMax, "\n")
    message("max count ", max(.bars_info$data$top), "\n")
            
    updateRanges <- function() {
        # contains c(x_min, x_max, y_min, y_max)
        if (horizontal) {
            if (is.null(xlim))
                .dataranges <<- c(make_data_ranges(c(.yMin, .yMax)),
                          make_data_ranges(c(xMinStartPosP(), xMaxEndPosP())))
            else
                .dataranges <<- c(make_data_ranges(c(.yMin, .yMax)), xlim) 
        }
        else {
            if (is.null(xlim))
                .dataranges <<- c(make_data_ranges(c(xMinStartPosP(),
                          xMaxEndPosP())), make_data_ranges(c(.yMin, .yMax)))
            else
                .dataranges <<- c(xlim, make_data_ranges(c(.yMin, .yMax))) 
        }
        # message("limits 2 ",.dataranges[1]," ", .dataranges[2]," ", .dataranges[3]," ", .dataranges[4], "\n")

    }
    updateRanges()
    
    
    
    #######################################################
    # Draw Axes
    coords <- function(item, painter, exposed) {
        
#        updateBarsInfo()
#        updateRanges()
#        updateLims()
        
        if (horizontal) {
            .ylab <<- name
            .xlab <<- "count"
        }
        else {
            .ylab <<- "count"
            .xlab <<- name
        }
        
        # grey background with grid lines
        horiPos <- .axis.loc(.dataranges[1:2])
        vertPos <- .axis.loc(.dataranges[3:4])
        #message("limits 3 ",.dataranges[1]," ", .dataranges[2]," ", .dataranges[3]," ", .dataranges[4], horiPos, vertPos, "\n")
        
        draw_grid_with_positions_fun(painter, .dataranges, horiPos, vertPos)

        # put labels
        draw_x_axes_with_labels_fun(painter, .dataranges, horiPos, horiPos, .xlab)
        draw_y_axes_with_labels_fun(painter, .dataranges, vertPos, vertPos, .ylab)            
        # title
        if (is.null(title)) title <- paste("Histogram of", name)
        add_title_fun(painter, .dataranges, title)
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
        
#        print(event$key())
        key <- event$key()
        
        if (key == Qt$Qt$Key_Up) {
            .type$binwidth <<- min(.type$binwidth * 1.1, maxBinwidthP())
            updateBarsInfo()

            #message("max count ", max(.bars_info$data$top), " .yMax ", .yMax, "\n")
            if (.yMax < max(.bars_info$data$top))
              .yMax <<- 1.1 * max(.bars_info$data$top)
            #message("Up ", .yMax, "\n")
            updateRanges()
            updateLims()
            #message("max count ", max(.bars_info$data$top), " .yMax ", .yMax, "\n")
           # message("limits ", .dataranges[3]," ", .dataranges[4], "\n")
            qupdate(.scene)
            qupdate(bglayer)
            qupdate(datalayer)
            scaleslayer$invalidateIndex()
        qupdate(scaleslayer)
        }
        else if (key == Qt$Qt$Key_Down) {
            .type$binwidth <<- .type$binwidth/1.1
            updateBarsInfo()
            if (.yMax < max(.bars_info$data$top))
              .yMax <<- 1.1 * max(.bars_info$data$top)        
            #message("Down ", .yMax, "\n")
            updateRanges()
            updateLims()
            qupdate(.scene)
            qupdate(bglayer)
            qupdate(datalayer)
            scaleslayer$invalidateIndex()
        qupdate(scaleslayer)
        }
        else if (key == Qt$Qt$Key_Left) {
            .type$start <<- .type$start - unitShiftP()
            # Make sure the start stays close to home
            if (.type$start < xMinStartPosP()) 
                .type$start <<- xMinStartPosP()
            updateBarsInfo()
            updateRanges()
            updateLims()
            qupdate(.scene)
            qupdate(bglayer)
            qupdate(datalayer)           
            scaleslayer$invalidateIndex()
        qupdate(scaleslayer)
        }
        else if (key == Qt$Qt$Key_Right) {
            .type$start <<- .type$start + unitShiftP()
            # Make sure the start stays close to home
            if (.type$start > xMaxStartPosP()) 
                .type$start <<- xMaxStartPosP()
            updateBarsInfo()
            updateRanges()
            updateLims()
            qupdate(.scene)
            qupdate(bglayer)
            qupdate(datalayer)                       
            scaleslayer$invalidateIndex()
            qupdate(scaleslayer)
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
            qupdate(scaleslayer)
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
#                print(.histOriginalBreaksAndStart)
                .type$type <<- "hist"
                .type$start <<- .histOriginalBreaksAndStart$start
                .type$binwidth <<- .histOriginalBreaksAndStart$binwidth
            }
            
        }
        else if (key == Qt$Qt$Key_M) {
 
          updateBarsInfo()
#          if (.yMax < max(.bars_info$data$top))
          .yMax <<- 1.1 * max(.bars_info$data$top)        
          #message("M ", .yMax, "\n")
          # print(.yMax)
          updateRanges()
          updateLims()
          qupdate(.scene)
          qupdate(bglayer)
          qupdate(datalayer)
          scaleslayer$invalidateIndex()
        qupdate(scaleslayer)
        }
        else if (key == 87) {
#            cat("\n\n\nClosing window!!!! - ", .view$close(), "\n")
        }
        
        if (key %in% c(Qt$Qt$Key_Up, Qt$Qt$Key_Down, Qt$Qt$Key_Left, Qt$Qt$Key_Right, 
            82, Qt$Qt$Key_M)) {
            #message("updating everything")
	    # updateBarsInfo()
            # qupdate(.scene)
            # qupdate(bglayer)
            # qupdate(datalayer)
            # qupdate(hoverlayer)
            # qupdate(brushing_layer)
        }
        
    }
    #######################################################
    # Brushing
    
    draw_brush_rect <- function(item, painter, exposed) {
#        cat("draw brush rect\n")
        left = min(.startBrush[1], .endBrush[1])
        right = max(.startBrush[1], .endBrush[1])
        top = max(.startBrush[2], .endBrush[2])
        bottom = min(.startBrush[2], .endBrush[2])
        
        qdrawRect(painter, left, bottom, right, top,
            fill = rgb(0, 0, 0, alpha = 0.7), stroke = "grey50")
#        cat("draw brush rect - done\n")
    }
    
    brushing_draw <- function(item, painter, exposed, ...) {
#        cat("brushing draw\n")
        if (.updateinfo) 
            updateBarsInfo()
        section <- subset(.bars_info$data, (.brushed > 0))
        
        if (nrow(section) > 0) {
            #  .brush.attr = attr(odata, '.brush.attr')
            # brushcolor <- .brush.attr[,'.brushed.color']
            brushColor <- brush(data)$color
            
            b <- section$bottom
            t <- (section$top - b) * section$.brushed + b
            
  #          cat("b: ")
  #          print(b)
  #          cat("t: ")
  #          print(t)
  #          cat("top: ")
  #          print(section$top)
  #          cat("perc: ")
  #          print(section$.brushed)
   #         cat("section:\n")
    #        print(.bars_info$data[, c("label", "group", "count", "top", "bottom", 
     #           ".brushed")])
            #cat('real data:\n'); print(data[data$disp < 200, c('disp', 'cyl',
            #   '.brushed')])
            
            brushColor <- rep(brushColor, nrow(section))
            brushColorRGBA <- col2rgb(brushColor, TRUE)
            #browser()
            if (brushColorRGBA["alpha", ][[1]] > 200) {
                brushColorRGBA["alpha", ][[1]] <- 200
                newBrushColor <- rgb(brushColorRGBA["red", ][[1]], brushColorRGBA["green", 
                  ][[1]], brushColorRGBA["blue", ][[1]], brushColorRGBA["alpha", 
                  ][[1]], maxColorValue = 255)
                
                rows <- section$.brushed < 1
                brushColor[rows] <- newBrushColor
            }
            
            if (horizontal) 
                qdrawRect(painter, b, section$right, t, section$left,
                          fill = brushColor, 
                  stroke = "grey50")
            else qdrawRect(painter, section$left, b, section$right, t,
                           fill = brushColor, 
                stroke = "grey50")
        }
        
        if (!is.null(.endBrush)) {
            draw_brush_rect(item, painter, exposed)
        }
#        cat("brushing draw - done\n")
    }
    
    brushing_mouse_press <- function(item, event, ...) {
#        cat("brushing mouse press\n")
        .brush <<- TRUE
        if (is.null(.startBrush)) {
            .startBrush <<- as.numeric(event$pos())
        }
        .endBrush <<- as.numeric(event$pos())
        
        setHiliting()
        qupdate(brushing_layer)
        #cat("brushing mouse press - done\n")
    }
    
    brushing_mouse_move <- function(item, event, ...) {
#        cat("brushing mouse move\n")
        .endBrush <<- as.numeric(event$pos())
        
        setHiliting()
        qupdate(brushing_layer)
#        cat("brushing mouse move - done\n")
    }
    
    brushing_mouse_release <- function(item, event, ...) {
#        cat("brushing mouse release\n")
        .endBrush <<- as.numeric(event$pos())
        setHiliting()
        qupdate(brushing_layer)
        
        
        .brush <<- FALSE
        .startBrush <<- NULL
        .endBrush <<- NULL
        
        setSelected()
#        cat("brushing mouse release - done\n")
    }
    
    setHiliting <- function() {
#        cat("setHiliting\n")
        
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
        
        # rows <<- (.bars_info$data$left <= right) & (.bars_info$data$right >= left) &
        #  (.bars_info$data$bottom <= top) & (.bars_info$data$top >= bottom)
        # .bars_info$data$.brushed <<- rows
        
        valid_bar_row <<- function(original, left, right, top, bottom) {
            val <- (left <= rightMouse) && (right >= leftMouse) && (min(bottom) <= 
                topMouse) && (max(top) >= bottomMouse)
            if (val) {
                1
            }
            else {
                # original
                0
            }
        }
        
        .bars_info$data <<- ddply(.bars_info$data, .(label), transform, .brushed = valid_bar_row(.brushed, 
            left, right, top, bottom))
        
       # print(head(subset(.bars_info$data, .brushed > 0)))
      #  cat("count of brushed sections: ", sum(.bars_info$data$.brushed), "\n")
     #   cat("count of left(<=", rightMouse, "): ", sum(.bars_info$data$left <= rightMouse), 
 #           "\n")
    #    cat("count of right(>= ", leftMouse, "): ", sum(.bars_info$data$right >= 
  #          leftMouse), "\n")
   #     cat("count of bottom(<= ", topMouse, "): ", sum(.bars_info$data$bottom <= 
   #         topMouse), "\n")
  #      cat("count of top(>= ", bottomMouse, "): ", sum(.bars_info$data$top >= bottomMouse), 
    #        "\n")
 #       cat("setHiliting - done\n")
    }
    
    setSelected <- function() {
#        cat("setSelected\n")
        section <- subset(.bars_info$data, .brushed == 1)
        
        rows <- rep(FALSE, nrow(data))
        if (NROW(section) > 0) 
            rows <- .data_col_names %in% as.character(section$label)
        
        # update original data
        data$.brushed <- rows
        # print(data[1:min(12, nrow(data)), ])
#        cat("setSelected - done\n")
    }
    
    
    #######################################################
    # Hover
    bar_hover_draw <- function(item, painter, exposed, ...) {
#        cat("\nBar Hover Draw\n")
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
        
        section <- subset(.bars_info$data, (y <= top) & (y >= bottom) & (x <= right) & 
            (x >= left))
        # print(head(section))
        
        # Nothing under mouse
        if (nrow(section) == 0) {
            .bar_hover_section <<- list(top = -1, bottom = 1, right = -1, left = 1)
            return()
        }
        
        
        # Highlight the rect
        brushColor <- brush(data)$color
        if (horizontal) {
            qdrawRect(painter, xleft = c(section$bottom),
                      ybottom = c(section$right), 
                xright = c(section$top), ytop = c(section$left),
                      stroke = brushColor, 
                fill = c(NA))
        }
        else {
            qdrawRect(painter, xleft = c(section$left),
                      ybottom = c(section$bottom), 
                xright = c(section$right), ytop = c(section$top),
                      stroke = brushColor, 
                fill = c(NA))
        }
        
        # Work out label text
        infostring <- paste("\nbin:", section[1, "label"], sep = " ")
        if (splitByCol != "qhist_split_column") {
            infostring <- paste(infostring, "group:", section[1, "group"], sep = " ")
        }
        
        count <- section$top - section$bottom
        infostring <- paste(infostring, "\ncount:", section[1, "count"], sep = " ")
        if (splitByCol != "qhist_split_column") {
            # nrow(section[]
            val <- section[1, "\ncount"]/sum(.bars_info$data[.bars_info$data$label %in% 
                section[1, "label"], "count"])
            if (is.null(val)) {
              if (val != 1) {
                  infostring <- paste(infostring, "\ncolumn proportion: ", pretty_percent(val), 
                    sep = "")
              }
            }
        }
        
        infostring <- paste(infostring, "\ndata proportion:", pretty_percent(section[1, 
            "\ncount"]/nrow(data)), sep = " ")
        
        
        
        # Label -> bin
        # count
        # proportion
        #
        # when split...
        # Label -> bin
        # count
        # column proportion
        # section proportion of column

        
#        qstrokeColor(painter) <- "black"
#        qdrawText(painter, infostring, .bar_queryPos[1], .bar_queryPos[2], valign = "top", 
#            halign = "left")
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

        .bar_hover_section <<- list(top = section$top, bottom = section$bottom, left = section$left, 
            right = section$right)
    }
    
    bar_hover <- function(item, event, ...) {
        # if (.brush) return()
        
        .bar_queryPos <<- as.numeric(event$pos())
        # qupdate(querylayer)
        
#        cat("\nBar Hover\n")
        
        if (horizontal) {
            x <- .bar_queryPos[2]
            y <- .bar_queryPos[1]
        }
        else {
            x <- .bar_queryPos[1]
            y <- .bar_queryPos[2]
        }
#        cat("x: ", x, " y: ", y, "\n")
#        cat("top: ", .bar_hover_section$top, " bottom: ", .bar_hover_section$bottom, 
#            " left: ", .bar_hover_section$left, " right: ", .bar_hover_section$right, 
#            "\n")
        
        if (!((y <= .bar_hover_section$top) & (y >= .bar_hover_section$bottom) & 
            (x <= .bar_hover_section$right) & (x >= .bar_hover_section$left))) {
            qupdate(hoverlayer)
        }
    }
    
    bar_leave <- function(item, event, ...) {
        # cat('\nBar Leave\n')
        # print(as.numeric(event$pos()))
        qupdate(hoverlayer)
    }
    #######################################################
    # scales
    scales_draw <- function(item, painter, exposed, ...) {
      # draw anchor point and line for bin width adjustment under first bin
      
      # anchor: 
      eps <- diff(windowRanges[3:4])/50
      qdrawSegment(painter, .bars_info$data$left[1], .bars_info$data$bottom[1]-eps/5, 
                            .bars_info$data$left[1], .bars_info$data$bottom[1]-eps,
                            "black")
      # bin width adjust: 
      qdrawSegment(painter, .bars_info$data$left[2], .bars_info$data$bottom[2]-eps/5, 
                            .bars_info$data$left[2], .bars_info$data$bottom[2]-eps,
                            "black")

      # max bin height cue area: 
      qdrawRect(painter, .dataranges[1],  .dataranges[4]-diff(.dataranges[3:4])/10, 
                         .dataranges[2], .dataranges[4], stroke=NA, fill=NULL
                        )

    }
    
    scales_handle_event <- function(pos) {
			xpos <- pos[1]
			ypos <- pos[2]
			xeps <- diff(.dataranges[1:2])/125
			yeps <- diff(.dataranges[3:4])/125
			
			rect = qrect(matrix(c(xpos-xeps, ypos-yeps, xpos + xeps, ypos + yeps), 2, byrow = TRUE))
      return(scaleslayer$locate(rect) + 1)
    }

    scales_hover <- function(item, event, ...) {

			hits <- scales_handle_event(as.numeric(event$pos()))
			
      if (length(hits) > 0) {
        hits <- hits[1]
        cu <- .view$cursor 
        switch(hits, {cursor <- Qt$Qt$SizeHorCursor},
          {
          cursor <- Qt$Qt$SizeHorCursor},
          {
          cursor <- Qt$Qt$UpArrowCursor}
        )
        cu$setShape(cursor)
        .view$cursor <- cu
#      print(hits)
      } else {
        cu <- .view$cursor 
        cu$setShape(Qt$Qt$ArrowCursor)      
        .view$cursor <- cu

#        event$ignore()
        bar_hover(item, event, ...)

      }
    }    

    scales_mouse_press <- function(item, event, ...) {
			hits <- scales_handle_event(as.numeric(event$pos()))
        
      if (length(hits) > 0) {
        updateBarsInfo()
        switch (hits, {}, {}, {
#          if (.yMax < max(.bars_info$data$top))
          .yMax <<- 1.1 * max(.bars_info$data$top)        
          #message("M ", .yMax, "\n")
          # print(.yMax)
          }
        )
        updateRanges()
        updateLims()
        qupdate(.scene)
        qupdate(bglayer)
        qupdate(datalayer)           
        scaleslayer$invalidateIndex()
        qupdate(scaleslayer)

      } else
        # pass mouse event on        
        brushing_mouse_press(item, event, ...)
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
#        print(sprintf("drag %s %s", pos[1], pos[2]))
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
        qupdate(.scene)
        qupdate(bglayer)
        qupdate(datalayer)           
        scaleslayer$invalidateIndex()
        qupdate(scaleslayer)
      
      } else {
        # pass mouse event on        
        brushing_mouse_move(item, event, ...)
      }
    }


    #######################################################
    # Layout
    updateLims <- function() {
        #message("update lims ", .dataranges[3]," ", .dataranges[4], "\n")
       
        windowRanges <- make_window_ranges(.dataranges, .xlab, .ylab)
        .lims <<- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])
        #message("update lims 2", windowRanges[3]," ", windowRanges[4], "\n")
        
        bglayer$setLimits(.lims)
        datalayer$setLimits(.lims)
        brushing_layer$setLimits(.lims)
        hoverlayer$setLimits(.lims)
        scaleslayer$setLimits(.lims)
    }

    windowRanges <- make_window_ranges(.dataranges, .xlab, .ylab)
    .lims <- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])
    
    .scene <- qscene()
    
    bglayer <- qlayer(.scene, coords, limits = .lims, clip = FALSE,
        keyPressFun = keyPressFun)
    datalayer <- qlayer(.scene, hist.all, limits = .lims, clip = FALSE)
    brushing_layer <- qlayer(.scene, brushing_draw,
        mousePressFun = brushing_mouse_press, 
        mouseMoveFun = brushing_mouse_move,
        mouseReleaseFun = brushing_mouse_release, 
        limits = .lims, clip = FALSE)
    

    hoverlayer <- qlayer(.scene, bar_hover_draw, limits = .lims, clip = FALSE,
        hoverMoveFun = bar_hover, hoverLeaveFun = bar_leave)

    scaleslayer <- qlayer(.scene, scales_draw, limits = .lims, clip = FALSE,
        mousePressFun = scales_mouse_press,
        mouseReleaseFun = scales_mouse_release,
        mouseMoveFun = scales_mouse_move,
        hoverMoveFun = scales_hover)
    
    # # update the brush layer in case of any modifications to the mutaframe
    #if (is.mutaframe(data)) {
    func <- function(i, j=NULL) {
        switch (j, .brushed = {
            qupdate(brushing_layer)
            updateBarsInfo()},
            {updateBarsInfo()
            if (.yMax < max(.bars_info$data$top)) {
                .yMax <<- 1.1 * max(.bars_info$data$top)
                #message("Data ",.yMax, "\n")
            }
            updateRanges()
            updateLims()
            #message("Data ",.lims[1],.lims[2], .lims[3], lims[4], "\n")
            qupdate(.scene)
            qupdate(bglayer)
            qupdate(brushing_layer)
            qupdate(datalayer)
            scaleslayer$invalidateIndex()
        qupdate(scaleslayer)
      })
    }
        
    add_listener(data, func)
    #}


    brush_update = function() {
        qupdate(brushing_layer)
    }
    .view <- qplotView(scene = .scene)
    .view
} 
