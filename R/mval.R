require(qtbase)
require(qtpaint)
require(plumbr)
require(plyr)

myvarsummary <- function(x) {
    if (is.factor(x) || is.character(x)) 
        return(names(sort(table(x), decreasing = TRUE))[1])
    if (is.logical(x)) 
        return(any(x, na.rm = TRUE))
    
    return(mean(x))
}

mysummary <- function(x) {
    ldply(x, myvarsummary)
}

scale_color <- function(colour, value = colour, na.color = 0) {
    if (is.numeric(colour)) {
        # assume grey colour scheme
        cmin <- min(colour, na.rm = T)
        cmax <- max(colour, na.rm = T)
        grey <- (value - cmin)/(cmax - cmin)
        grey <- pmin(grey, 1)
        grey <- pmax(grey, 0)
        nas <- is.na(grey)
        grey[nas] <- na.color
        return(rgb(grey, grey, grey))
    }
    
    print(paste("colour not implemented for type", mode(colour)))
}


##' Interactive Missing Value Plot.
##'
##' @param data a mutaframe which is typically built upon a data frame
##' along with several row attributes
##' @param ...
##' @return NULL
##' @author Heike Hofmann
##' @export
##' @example cranvas/inst/examples/maps-ex.R
qmval <- function(data, vars, main, varmax = 20, ...) {
    ## check if an attribute exist
    #  browser()
    #\tif (!is.mutaframe(data)) data <- qdata(data)
    if (!(".brushed" %in% names(data))) 
        data$.brushed <- FALSE
    
    #  ## parameters for the brush
    #  .brush.attr = attr(data, '.brush.attr')
    
    ##
    if (missing(vars)) 
        vars <- names(data)
    if (is.numeric(vars)) 
        vars = names(data)[as.integer(vars)]
    
    ## we are only interested in variables with missing values
    df.data <- as.data.frame(data[, vars])
    nmis <- sapply(df.data, function(x) return(sum(is.na(x))), simplify = T)
    vars <- vars[nmis > 0]
    ## use only varmax many variables
    if (length(vars) > varmax) {
        print(paste("warning: only first", varmax, "variables out of", length(vars), 
            "are shown."))
        vars <- vars[1:varmax]
    }
    # get data summary
    .data.summary <- ldply(df.data[, vars], function(x) return(sum(is.na(x))))
    names(.data.summary) <- c("Names", "NAs")
    .data.summary$Values <- nrow(df.data) - .data.summary$NAs
    .data.summary$mvBrushed <- 0
    .data.summary$valBrushed <- 0
    
    recalcBrushing <- function() {
        # update .data.summary$mvBrushed
        .data.summary$mvBrushed <<- ldply(df.data[, .data.summary$Names], function(x) return(sum(is.na(x) & 
            data$.brushed)))$V1
        .data.summary$valBrushed <<- ldply(df.data[, .data.summary$Names], function(x) return(sum(!is.na(x) & 
            data$.brushed)))$V1
    }
    
    recalcBrushing()
    
    p <- length(vars)
    
    dataRanges <- c(make_data_ranges(c(0, 1)), make_data_ranges(c(0, 1)))
    
    # space in window around plot (margins in base R)
    # this space depends on the labels needed on the left
    # find out about these first:
    
    dataname <- deparse(substitute(data))
    if (missing(main)) {
        main <- paste("Missing Value Plot of", dataname)
    }
    windowRanges <- make_window_ranges(dataRanges, "", "", ytickmarks = "", main = main)
    
    lims <- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])
    
    
    draw <- function(item, painter, exposed) {
        # basic rectangle:
        top <- 0.1
        left <- 0
        right <- 1
        bottom <- 0.1 + 0.6 * 1/p
        
        n <- nrow(data)
        for (i in 1:p) {
            qdrawRect(painter, left, bottom, right * .data.summary$Values[i]/n, top, 
                fill = "grey50", stroke = "black")
            qdrawRect(painter, right * .data.summary$Values[i]/n, bottom, right, 
                top, fill = "white", stroke = "black")
            # qdrawText(painter, .data.summary$Names[i], left, bottom, halign = 'left',
            #   valign = 'bottom')
            bottom <- bottom + 1/p
            top <- top + 1/p
        }
        
        # put labels on top
        bottom <- 0.1
        for (i in 1:p) {
            qdrawText(painter, .data.summary$Names[i], left, bottom, halign = "left", 
                valign = "bottom")
            bottom <- bottom + 1/p
        }
        
        add_title_fun(painter, dataRanges, title = main)
        
        #    datalayer <<- item
    }
    
    # Brushing -----------------------------------------------------------------
    .startBrush <- NULL
    .endBrush <- NULL
    .brush <- FALSE
    
    drawBrush <- function(item, painter, exposed) {
        left = min(.startBrush[1], .endBrush[1])
        right = max(.startBrush[1], .endBrush[1])
        top = max(.startBrush[2], .endBrush[2])
        bottom = min(.startBrush[2], .endBrush[2])
        
        qdrawRect(painter, left, bottom, right, top, fill = rgb(0, 0, 0, alpha = 0.3), 
            stroke = "black")
    }
    
    
    brushing_draw <- function(item, painter, exposed, ...) {
        
        brushcolor <- brush(data)$color
        
        # basic rectangle:
        top <- 0.1
        left <- 0
        right <- 1
        bottom <- 0.1 + 0.6 * 1/p
        
        n <- nrow(data)
        for (i in 1:p) {
            qdrawRect(painter, left, bottom, (right * .data.summary$valBrushed[i])/n, 
                top, fill = brushcolor, stroke = brushcolor)
            qdrawRect(painter, (right * (n - .data.summary$mvBrushed[i]))/n, bottom, 
                right, top, fill = brushcolor, stroke = brushcolor)
            bottom <- bottom + 1/p
            top <- top + 1/p
        }
        
        # put labels on top
        bottom <- 0.1
        qstrokeColor(painter) <- "black"
        for (i in 1:p) {
            qdrawText(painter, .data.summary$Names[i], left, bottom, halign = "left", 
                valign = "bottom")
            bottom <- bottom + 1/p
        }
        
        if (!is.null(.endBrush)) {
            drawBrush(item, painter, exposed)
        }
    }
    
    brushing_mouse_press <- function(item, event, ...) {
        #print('brushing_mouse_press')
        .brush <<- TRUE
        if (is.null(.startBrush)) {
            .startBrush <<- as.numeric(event$pos())
            .endBrush <<- as.numeric(event$pos())
        }
        
        setHiliting()
        qupdate(brushing_layer)
    }
    
    brushing_mouse_move <- function(item, event, ...) {
        #print('brushing_mouse_move')
        .endBrush <<- as.numeric(event$pos())
        
        setHiliting()
        qupdate(brushing_layer)
    }
    
    brushing_mouse_release <- function(item, event, ...) {
        #print('brushing_mouse_release')
        .endBrush <<- as.numeric(event$pos())
        setHiliting()
        qupdate(brushing_layer)
        
        
        .brush <<- FALSE
        
        
        .startBrush <<- NULL
        .endBrush <<- NULL
        
        setSelected()
    }
    
    setHiliting <- function() {
        left = min(.startBrush[1], .endBrush[1])
        right = max(.startBrush[1], .endBrush[1]) + 1e-08
        top = max(.startBrush[2], .endBrush[2]) + 1e-08
        bottom = min(.startBrush[2], .endBrush[2])
        
        rect = qrect(matrix(c(left, bottom, right, top), 2, byrow = TRUE))
        hits = datalayer$locate(rect) + 1
        #\t\tbrowser()
        for (i in 1:p) {
            .data.summary$mvBrushed[i] <<- 0
            .data.summary$valBrushed[i] <<- 0
        }
        for (i in hits) {
            var <- (i + 1)%/%2
            missing <- (i%%2) == 0
            
            if (missing) 
                .data.summary$mvBrushed[var] <<- .data.summary$NAs[var]
            else .data.summary$valBrushed[var] <<- .data.summary$Values[var]
        }
    }
    
    setSelected <- function() {
        .brushed <- rep(FALSE, nrow(data))
        #\t\tbrowser()
        for (i in 1:p) {
            if (.data.summary$mvBrushed[i] == .data.summary$NAs[i]) {
                print(.data.summary$Names[i])
                .brushed[which(is.na(data[, .data.summary$Names[i]]))] <- TRUE
            }
            if (.data.summary$valBrushed[i] == .data.summary$Values[i]) {
                print(.data.summary$Names[i])
                .brushed[which(!is.na(data[, .data.summary$Names[i]]))] <- TRUE
            }
        }
        data$.brushed <- .brushed
    }
    
    
    # Key board events ---------------------------------------------------------
    
    keyPressFun <- function(item, event, ...) {
        #\t\tif (event$key() == Qt$Qt$Key_Shift) .extended <<- !.extended
        if (event$key() == Qt$Qt$Key_S) {
            # sort according to number missing values
            .data.summary <<- .data.summary[order(.data.summary$Values), ]
            datalayer$invalidateIndex()
            qupdate(datalayer)
            qupdate(brushing_layer)
        }
    }
    
    
    # Display category information on hover (query) ----------------------------
    .queryPos <- NULL
    
    query_draw <- function(item, painter, exposed, ...) {
        # Don't draw when brushing
        if (.brush) 
            return()
        if (is.null(.queryPos)) 
            return()
        xpos <- .queryPos[1]
        ypos <- .queryPos[2]
        rect = qrect(matrix(c(xpos, ypos, xpos + 1e-04, ypos + 1e-04), 2, byrow = TRUE))
        hits = datalayer$locate(rect) + 1
        
        # Nothing under mouse?
        if (length(hits) == 0) 
            return()
        
        var <- (hits + 1)%/%2
        info <- .data.summary[var, ]
        infostring = with(info, paste(Names[1], ": ", valBrushed[1], "/", Values[1], 
            " NAs: ", mvBrushed[1], "/", NAs[1]), sep = "")
        
        bgwidth = qstrWidth(painter, infostring)
        bgheight = qstrHeight(painter, infostring)
        
        ## adjust drawing directions when close to the boundary
        hflag = windowRanges[2] - xpos > bgwidth
        vflag = ypos - windowRanges[3] > bgheight
        qdrawRect(painter, xpos, ypos, xpos + ifelse(hflag, 1, -1) * bgwidth, ypos + 
            ifelse(vflag, -1, 1) * bgheight, stroke = rgb(1, 1, 1, 0.5), fill = rgb(1, 
            1, 1, 0.5))
        
        qstrokeColor(painter) = brush(data)$label.color
        qdrawText(painter, infostring, xpos, ypos, halign = ifelse(hflag, "left", 
            "right"), valign = ifelse(vflag, "top", "bottom"))
    }
    
    query_hover <- function(item, event, ...) {
        if (.brush) 
            return()
        
        .queryPos <<- as.numeric(event$pos())
        qupdate(querylayer)
    }
    
    query_hover_leave <- function(item, event, ...) {
        .queryPos <<- NULL
        qupdate(querylayer)
    }
    
    coords <- function(item, painter, exposed) {
    }
    
    # Display legend information for colour ----------------------------
    
    legend_draw <- function(item, painter, exposed, ...) {
        
    }
    
    scene = qscene()
    bglayer = qlayer(scene, coords, limits = lims, clip = FALSE)
    datalayer = qlayer(scene, draw, keyPressFun = keyPressFun, limits = lims, clip = FALSE)
    brushing_layer = qlayer(scene, brushing_draw, mousePressFun = brushing_mouse_press, 
        mouseMoveFun = brushing_mouse_move, mouseReleaseFun = brushing_mouse_release, 
        limits = lims, clip = FALSE)
    querylayer = qlayer(scene, query_draw, hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave, 
        limits = lims, clip = FALSE)
    legendlayer = qlayer(scene, legend_draw, limits = lims, clip = FALSE)
    
    
    
    ## update the brush layer in case of any modifications to the mutaframe
    add_listener(data, function(i, j) {
        switch(j, .brushed = {
            recalcBrushing()
            qupdate(brushing_layer)
        }, .color = {
            qupdate(datalayer)
            qupdate(brushing_layer)
        })
    })
    
    
    #\t## update the brush layer if brush attributes change
    #\tadd_listener(.brush.attr, function(i, j) {
    #\t\t\tqupdate(brushing_layer)
    #\t})
    
    qplotView(scene = scene)
}
 
