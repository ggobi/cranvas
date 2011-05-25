

paste_formula <- function(form) {
    # form has pieces wt, marg and cond
    # output is character - needs to be converted to formula afterwards
    wtStr <- ""
    if (length(form$wt) > 0) 
        wtStr <- form$wt[1]
    margStr <- "1"
    if (length(form$marg) > 0) 
        margStr <- paste(form$marg, collapse = "+")
    condStr <- ""
    if (length(form$cond) > 0) 
        condStr <- paste("|", paste(form$cond, collapse = "+"))
    
    formstring <- paste(wtStr, "~", margStr, condStr)
    return(formstring)
}

extract_formula <- function(formula) {
    form <- parse_product_formula(formula)
    form$marg <- setdiff(form$marg, c(".brushed", ".fill"))
    
    return(paste_formula(form))
}


addDivider <- function(divider, level = length(divider)) {
    # adds a spine divider orthogonal to the previous direction
    if (is.function(divider)) 
        return(divider)
    
    if (level > length(divider)) 
        level <- length(divider)
    dvd <- rev(rev(divider)[1:level])
    if (dvd[1] %in% c("hspine", "hbar")) 
        dividerhil <- c("vspine", dvd)
    else if (dvd[1] %in% c("vspine", "vbar")) 
        dividerhil <- c("hspine", dvd)
    return(dividerhil)
}

qprodcalc <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE) {
	res <- prodcalc(data, formula, divider, cascade, scale_max, na.rm)
# multiply by 100 to get out of the [0,1] range - that plays less havoc with the pretty axes
	res[,c("l","t","b","r")] <- 100 * res[,c("l","t","b","r")]
	return(res)
}

##' Mosaic plot.
##' Create a mosaicplot using a formula (as described in prodplot)
##'
##' up arrow - takes out top var, 
##' down arrow - adds next variable (if available)
##' r toggles top variable between horizontal and vertical direction
##' b/s changes top variable between (b)archarts and (s)pine plots
##' u/c (un)conditions on the bottom variable
##' @param data a mutaframe which is typically built upon a data frame
##' along with several row attributes
##' @param formula a formula to describe order in which variables go into the mosaicplot. The first variables are the ones visually most important, i.e. Y ~ X1 + X2 + X3 first splits according to X3, then according to X2, then according to X1
##' @param divider structure of the split in each direction. Choices are 'hbar', 'vbar' for horizontal/vertical barcharts, 'hspine', 'vspine' for horizontal/vertical spineplots.
##' @param cascade parameter for prodplot in package productplots
##' @param scale_max parameter for prodplot in package productplots
##' @param na.rm handling of missing values, defaults to FALSE
##' @param subset parameter for prodplot -
##' @param colour fill colour of rectangles - only used if colour is not used in the data
##' @param main parameter for prodplot
##' @param cache boolean to turn cache on for layers, defaults to TRUE
##' @param ...
##' @return NULL
##' @author Heike Hofmann
##' @export
##' @example cranvas/inst/examples/mosaic-ex.R
qmosaic <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, 
    na.rm = FALSE, subset = NULL, colour = "grey30", main = NULL, cache= T, ...) {
    ## check if an attribute exist
    has_attr = function(attr) {
        attr %in% names(data)
    }
    
    ## parameters for the updating events
    .recalc <- FALSE
    .recalchiliting <- FALSE
    
    
    .filled <- TRUE
    .formula <- formula
    form <- parse_product_formula(.formula)
    .activevars <- c(form$marg, form$cond)
    .ncond <- length(form$cond)
    .inactivevars <- NULL
    
    if (is.function(divider)) 
        divider <- divider(length(.activevars))
    .divider <- divider
    
    recalchiliting <- function() {
        ## depends on .formula and data
        
        ## hilite setup
        #print('recalchiliting')
        
        form <- parse_product_formula(.formula)
        form$marg <- c(".brushed", setdiff(form$marg, ".fill"))
        hformula <- as.formula(paste_formula(form))
        
        hdivider <- addDivider(tail(divider, length(.activevars)))
        #print(hdivider)
        
        #print('hformula')
        #print(hformula)
        #print(summary(data$.brushed))
        ## calculate hilited bins
        df <- data.frame(data)
        df$.brushed <- data$.brushed
        df$.brushed <- factor(df$.brushed, levels = c("TRUE", "FALSE"))
        helperdata <- qprodcalc(df, hformula, hdivider, cascade, scale_max, na.rm = na.rm)
        hdata <<- subset(helperdata, (.brushed == TRUE), drop = FALSE)
        .recalchiliting <<- FALSE
        ##print(summary(helperdata))
    }
    
    recalc <- function() {
        # depends on .formula
        # sets xdata and bkdata
        # (bkdata is equal to xdata without color, if color is used, bkdata is step
        #   before color)
        # all.data contains all steps of hierarchical build
        #print('recalc')
        
        .divider <- tail(divider, length(.activevars))
        ## color setup
        if (.filled) {
            form <- parse_product_formula(.formula)
            # move .fill variable to the front, whereever it was before
            # it probably should not be part of .formula - that's the plan at least
            form$marg <- c(".fill", setdiff(form$marg, ".fill"))
            
            .formula <- as.formula(paste_formula(form))
            .divider <- addDivider(divider)
        }
        
        ## calculate all bin sizes & positions
        df <- data.frame(data)
        all.data <<- qprodcalc(df, .formula, .divider, cascade, scale_max, na.rm = na.rm)
        ##print(summary(all.data))
        # keep only highest level
        if (is.null(all.data$.brushed)) 
            all.data$.brushed <<- FALSE
        
        xdata <<- subset(all.data, level == max(all.data$level), drop = FALSE)
        bkdata <<- subset(all.data, level == max(all.data$level) - .filled, drop = FALSE)

        
        .recalc <<- FALSE
        recalchiliting()
    }
    
    ## initialize cross-tabulated data
    all.data <- NULL
    xdata <- NULL
    bkdata <- NULL
    
    ## initialize hiliting data hdata
    hdata <- NULL
    # recalc calls recalchiliting
    recalc()
    
    top <- xdata$t
    bottom <- xdata$b
    left <- xdata$l
    right <- xdata$r
    
    if (is.null(main)) 
        .df.title <- TRUE
    xlab <- find_x_label(xdata)
    ylab <- find_y_label(xdata)
    dataRanges <- c(make_data_ranges(c(min(left, na.rm = T), max(right, na.rm = T))), 
        make_data_ranges(c(min(bottom, na.rm = T), max(top, na.rm = T))))
    # space in window around plot (margins in base R)
    # this space depends on the labels needed on the left
    # find out about these first:
    
    row <- find_row_level(xdata)
    ylabels <- NULL
    if (!is.na(row)) 
        ylabels <- row_labels(xdata[xdata$level == row, ])
    
    
    if (.df.title) {
        main <- extract_formula(.formula)
        #  print(main)
    }
    windowRanges <- make_window_ranges(dataRanges, xlab, ylab, ytickmarks = !is.null(ylabels), 
        main = main)
    
    lims <- qrect(windowRanges[c(1, 2)], windowRanges[c(3, 4)])
    
    coords <- function(item, painter, exposed) {
        sx <- scale_x_product(xdata)
				if (max(sx$breaks) <= 1) {
					sx$breaks <- 100*sx$breaks 	# needed because of scaling in qprodcalc
				}
        sy <- scale_y_product(xdata)
				if (max(sy$breaks) <= 1) {
					sy$breaks <- 100*sy$breaks	# needed because of scaling in qprodcalc
				}
        
        # grey background with grid lines
        draw_grid_with_positions_fun(painter, dataRanges, sx$breaks, sy$breaks,  minor.horiPos=NULL, 
            sy$minor_breaks) # no minor axes on x
        
        # put labels, if appropriate
        col <- find_col_level(xdata)
        if (!is.na(col)) {
            labels <- col_labels(xdata[xdata$level == col, ])
            
            draw_x_axes_with_labels_fun(painter, dataRanges, axisLabel = labels$label, 
                labelHoriPos = labels$pos, name = xlab)
        }
        else {
            draw_x_axes_with_labels_fun(painter, dataRanges, axisLabel = rep("", 
                length(sx$breaks)), labelHoriPos = sx$breaks, name = xlab)
        }
        
        if (!is.na(row)) {
            labels <- row_labels(xdata[xdata$level == row, ])
            draw_y_axes_with_labels_fun(painter, dataRanges, axisLabel = labels$label, 
                labelVertPos = labels$pos, name = ylab)
        }
        else {
            draw_y_axes_with_labels_fun(painter, dataRanges, axisLabel = rep("", 
                length(sy$breaks)), labelVertPos = sy$breaks, name = ylab)
        }
    }
    
    mdraw <- function(item, painter, exposed) {
        # extract data at level .level and draw
        #print('mdraw: full mosaic drawn')
        ##print(summary(xdata))
        if (.recalc) 
            recalc()
        
        
        top <- xdata$t
        bottom <- xdata$b
        left <- xdata$l
        right <- xdata$r
        if (.filled) 
            color <- as.character(xdata$.fill)
        else color <- colour
        
        qdrawRect(painter, left, bottom, right, top, fill = color)
        
        if (.df.title) {
            add_title_fun(painter, dataRanges, title = extract_formula(.formula))
        }
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
        if (.recalchiliting) 
            recalchiliting()
        
        if (.brush) {
            #print(names(xdata))
            hdata <<- subset(bkdata, .brushed == TRUE)
            print(names(bkdata))
            
        }
        if (nrow(hdata) > 0) {
            top <- hdata$t
            bottom <- hdata$b
            left <- hdata$l
            right <- hdata$r
            
            #  .brush.attr = attr(odata, '.brush.attr')
            
            brushcolor <- brush(data)$color
            qdrawRect(painter, left, bottom, right, top, fill = brushcolor)
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
        right = max(.startBrush[1], .endBrush[1])
        top = max(.startBrush[2], .endBrush[2])
        bottom = min(.startBrush[2], .endBrush[2])
        
        # use .level - .filled to select all bins, independently of color
        # if colored bins should be available for highlighting separately use level ==
        #   .level
        
        bkdata$.brushed <<- (bkdata$l <= right) & (bkdata$r >= left) & (bkdata$b <= 
            top) & (bkdata$t >= bottom)
    }
    
    setSelected <- function() {
        # propagate highlighting to the data set and other plots
        
        print(names(bkdata))
        print(.activevars)
        hdata <- subset(bkdata, (.brushed == TRUE), drop = FALSE)[, .activevars, 
            drop = FALSE]
        
        if (nrow(hdata) > 0) {
            hdata$ID <- 1:nrow(hdata)
            res.melt <- melt(hdata, id.var = "ID")
            res.cond <- adply(res.melt, 1, function(x) {
                if (is.na(x$value)) 
                  cstr <- paste("is.na(", x$variable, ")", sep = "")
                else cstr <- paste("(", x$variable, "=='", x$value, "')", sep = "")
                return(cond = cstr)
            })
            res.cond <- res.cond[, -3]
            names(res.cond)[3] <- "value"
            cast.res <- cast(res.cond, ID ~ ., function(x) return(paste(x, collapse = " & ")))
            
            cond1 <- paste("(", cast.res[, 2], ")", sep = "", collapse = " | ")
            idx <- with(data.frame(data), which(eval(parse(text = cond1))))
            
            .brushed <- rep(FALSE, nrow(data))
            if (length(idx)) 
                .brushed[idx] <- TRUE
            
            data$.brushed <- .brushed
        }
        else {
            data$.brushed <- FALSE
        }
    }
    
    # Key board events ---------------------------------------------------------
    
    keyPressFun <- function(item, event, ...) {
        #print(event$key())
        key <- event$key()
        
        datachanged <- FALSE
        formulachanged <- FALSE
        form <- parse_product_formula(.formula)
        form$marg <- setdiff(form$marg, c(".fill", ".brushed"))
        
        if (key == Qt$Qt$Key_Up) {
            # arrow up
            # remove marginal variables first
            if (length(.activevars) > 1) {
                if ((length(form$marg) > 0) && (form$marg[1] != "1")) {
                  dmvar <- form$marg[1]
                  form$marg <- form$marg[-1]
                }
                else if (length(form$cond) > 1) {
                  dmvar <- form$cond[1]
                  form$cond <- form$cond[-1]
                }
                .inactivevars <<- c(dmvar, .inactivevars)
                .activevars <<- setdiff(.activevars, dmvar)
                #    .level <<- .level-1
            }
            else return()
        }
        else if (key == Qt$Qt$Key_Down) {
            # arrow down
            if (!is.null(.inactivevars)) {
                if (length(form$cond) < .ncond) {
                  form$cond <- c(form$cond, .inactivevars[1])
                }
                else {
                  form$marg <- c(.inactivevars[1], form$marg)
                }
                .activevars <<- c(.activevars, .inactivevars[1])
                .inactivevars <<- .inactivevars[-1]
                
                #       .level <<- .level + 1
            }
            else return()
        }
        else if (key == Qt$Qt$Key_Left) {
            # arrow left
            if (length(.inactivevars) > 0) {
                .inactivevars <<- c(.inactivevars, form$marg[1])
                .activevars <<- c(setdiff(.activevars, form$marg[1]), .inactivevars[1])
                form$marg[1] <- .inactivevars[1]
                .inactivevars <<- .inactivevars[-1]
            }
            else return()
        }
        else if (key == Qt$Qt$Key_Right) {
            # arrow right
            if (length(.inactivevars) > 0) {
                lastone <- length(.inactivevars)
                .inactivevars <<- c(.inactivevars, form$marg[1])
                .activevars <<- c(setdiff(.activevars, form$marg[1]), .inactivevars[lastone])
                form$marg[1] <- .inactivevars[lastone]
                .inactivevars <<- .inactivevars[-lastone]
            }
            else return()
        }
        else if (key == Qt$Qt$Key_R) {
            # 'r' or 'R' for 'rotate'
            lindx <- length(divider) - length(.activevars) + 1
            
            if (divider[lindx] %in% c("hbar", "vbar")) 
                divider[lindx] <<- setdiff(c("hbar", "vbar"), divider[lindx])
            if (divider[lindx] %in% c("hspine", "vspine")) 
                divider[lindx] <<- setdiff(c("hspine", "vspine"), divider[lindx])
        }
        else if (key == Qt$Qt$Key_U) {
            # 'u' or 'U' for 'unconditioning'
            if (length(form$cond) > 0) {
                # take first conditioning variable and use as last marginal variable
                
                form$marg <- c(form$marg, form$cond[1])
                form$cond <- form$cond[-1]
            }
            else return()
        }
        else if (key == Qt$Qt$Key_C) {
            # 'c' or 'C' for 'conditioning'
            if ((length(form$marg) > 0) & (form$marg[1] != "1")) {
                # take last marginal variable and use as first condition
                lastone <- length(form$marg)
                form$cond <- c(form$marg[lastone], form$cond)
                form$marg <- form$marg[-lastone]
            }
            else return()
        }
        else if (key == Qt$Qt$Key_S) {
            # 's' or 'S' for 'spine'
            lindx <- length(divider) - length(.activevars) + 1
            
            divider[lindx] <<- gsub("bar", "spine", divider[lindx])
        }
        else if (key == Qt$Qt$Key_B) {
            # 'b' or 'B' for 'bar'
            lindx <- length(divider) - length(.activevars) + 1
            
            divider[lindx] <<- gsub("spine", "bar", divider[lindx])
        }
        #    if (.filled) {
        .formula <<- as.formula(paste_formula(form))
        recalc()
        #     recalchiliting()
        #    }
        .activevars <<- na.omit(.activevars)
        qupdate(bglayer)
        qupdate(datalayer)
        qupdate(brushing_layer)
    }
    
    
    # Display category information on hover (query) ----------------------------
    .queryPos <- NULL
    
    query_draw <- function(item, painter, exposed, ...) {
        # Don't draw when brushing
        if (.brush) 
            return()
        if (is.null(.queryPos)) 
            return()
        
        xpos <- x <- .queryPos[1]
        ypos <- y <- .queryPos[2]
        
        info <- subset(xdata, (y <= t) & (y >= b) & (x <= r) & (x >= l))
        #      (level == .level))
        
        # Nothing under mouse
        if (nrow(info) == 0) 
            return()
        
        # Work out label text
        idx <- setdiff(names(xdata), c("l", "t", "r", "b", ".wt", "level", ".brushed", 
        	".fill", .inactivevars))
        infodata <- as.character(unlist(info[1, idx]))
        infostring <- paste(idx, infodata, collapse = "\n", sep = ":  ")
#browser()        
        infostring <- paste("\n", infostring)
				if (info[1,".wt"] > 1) 
					infostring <- paste(infostring, sprintf("\ncount:  %s", info[1,".wt"]))
				else {
					infostring <- paste(infostring, sprintf("\nPct:  %s%%", round(100*info[1,".wt"], 1)))
				}
        #    qstrokeColor(painter) <- 'white'
        #    qdrawText(painter, infostring, x, y, valign='top', halign='left')
        bgwidth = qstrWidth(painter, infostring)
        bgheight = qstrHeight(painter, infostring)
        
        ## adjust drawing directions when close to the boundary
        hflag = windowRanges[2] - xpos > bgwidth
        vflag = ypos - windowRanges[3] > bgheight
        qdrawRect(painter, xpos, ypos, xpos + ifelse(hflag, 1, -1) * bgwidth, ypos + 
            ifelse(vflag, -1, 1) * bgheight, stroke = rgb(1, 1, 1, 0.95), fill = rgb(1, 
            1, 1, 0.95))
        
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
    
    
    scene = qscene()
    bglayer = qlayer(scene, coords, limits = lims,  cache = cache)
    datalayer = qlayer(scene, mdraw, limits = lims, cache = cache)
    brushing_layer = qlayer(scene, brushing_draw, mousePressFun = brushing_mouse_press, 
        mouseMoveFun = brushing_mouse_move, mouseReleaseFun = brushing_mouse_release, 
        keyPressFun = keyPressFun, limits = lims,  cache = cache)
    querylayer = qlayer(scene, query_draw, limits = lims,  hoverMoveFun = query_hover, 
        hoverLeaveFun = query_hover_leave, cache = cache)
    
    
    ## update the brush layer in case of any modifications to the mutaframe
    add_listener(data, function(i, j) {
        switch(j, .brushed = {
            .recalchiliting <<- TRUE
            # recalchiliting should be called but at this point, data is not yet updated -
            #   it will be updated before it's called by the qupdate function, though
            #     recalchiliting()
            qupdate(brushing_layer)
        }, .fill = {
            .recalc <<- TRUE
            #     recalc()
            #           recalchiliting()
            qupdate(bglayer)
            qupdate(datalayer)
            qupdate(brushing_layer)
        })
    })
    
    
    ## update the brush layer if brush attributes change
    # add_listener(.brush.attr, function(i, j) {
    #   qupdate(brush_layer)
    # })
    
    qplotView(scene = scene)
}
 
