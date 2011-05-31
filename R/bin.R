#' Make dodge positions
#'
#' @param breaks break positions
#' @param n number of items per break
#' @keywords internal
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @examples
#'  make_dodge_pos(c(1:5), 3)
make_dodge_pos <- function(breaks, n) {
    gap <- diff(breaks[1:2])
    breaks <- breaks[-length(breaks)]

    relPos <- seq(from = gap * 0.1, to = gap * 0.9, length.out = n + 1)
    startRel <- relPos[-(n + 1)]
    endRel <- relPos[-1]

    starts <- c(sapply(startRel, function(x) {
        x + breaks
    }))
    ends <- c(sapply(endRel, function(x) {
        x + breaks
    }))

    data.frame(start = starts, end = ends)
}


#' Fill and Stroke by Color
#' Set the fill and stroke by the color if they are already not defined
#'
#' @param color color to be used for (possibly) both the fill and stroke
#' @param fill fill to be used
#' @param color stroke to be used
#' @author Barret Schloerke 
#' @keywords internal
#' @examples
#'   fill_and_stroke(color = 'red')
#'   fill_and_stroke(fill = 'red', stroke = 'black')
#'   fill_and_stroke(color = 'red', stroke = 'black')
#'   fill_and_stroke(color = 'red', fill = 'black')
fill_and_stroke <- function(color = NULL, fill = NULL, stroke = NULL) {
    if (is.null(fill))
        fill <- color
    if (is.null(stroke)) { # lighter outline, darker fill
    	#rgbfill = col2rgb(fill)
        #stroke = rgb(rgbfill["red",]*2, rgbfill["green",]*2,
        #  rgbfill["blue",]*2, maxColorValue=255)
        stroke <- color
    }
    list(fill = fill, stroke = stroke)
}

#' Divide by maximum.
#' Divides a vector by a maximum value of a vector
#'
#' @param val value to be scaled
#' @param maxVal vector to be used for finding the maximum value
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords internal
#' @examples
#' divide_by_maximum(1:10)
#' divide_by_maximum(1:10, 1:20)
divide_by_maximum <- function(val, maxVal = val) {
    maxValue <- max(maxVal)
    if (maxValue != 0)
        val/maxValue
    else val
}


percent_of_brushed <- function(left, right, dataValue, brushVal) {
    # print(left)
    # print(right)
    # print(dataValue)
    # print(brushVal)
    rows <- dataValue > left & dataValue <= right
    sumB <- sum(brushVal[rows])
    lengthB <- sum(rows)

    perc <- sumB/lengthB

    perc
}


#' Continuous items to bins
#'
#' @param data data to be used
#' @param splitBy vect to split by
#' @param brushed vect to brush by
#' @param typeInfo typeInfo$type ENUM of 'hist', 'ash', 'dot', 'spine', 'density'
#' @param position enum{'none', 'stack', 'dodge', 'relative', 'identity'}
#' @param color vect to color by
#' @param fill vect to fill by
#' @param stroke vect to outline by
#' @param ... other params passed to \code{\link[graphics]{hist}}
#' @author Barret Schloerke 
#' @keywords internal
#' @examples
#' temp_breaks <- hist(mtcars$disp, plot=FALSE)$breaks[1:2]
#' type <- list(type = 'hist', binwidth = diff(temp_breaks), start = temp_breaks[1])
#' continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, stroke = 'black')
#' continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, position = 'dodge', stroke = 'black')
#' continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, position = 'identity', stroke = 'black')
#' continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, position = 'relative', stroke = 'black')
#' continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, position = 'stack', stroke = 'black')
continuous_to_bars <- function(data = NULL, splitBy = NULL, brushed = NULL,
    typeInfo = "hist", position = "none", color = NULL, fill = NULL,
    stroke = NULL, ...) {
    ignore <- substitute(...)
    if (any(is.na(data))) data <- na.omit(data)

    original = list(data = data, splitBy = splitBy, color = color,
      stroke = stroke, fill = fill, position = position)

    if (identical(typeInfo$type, "hist")) {
      #  message("making a hist")
    } else if (identical(typeInfo$type, "ash"))
        stop("ash not defined yet")
    else if (identical(typeInfo$type, "dot"))
        stop("dot not defined yet")
    else if (identical(typeInfo$type, "spine"))
        stop("spine-o-gram not defined yet")
    else if (identical(typeInfo$type, "dot"))
        stop("dot not defined yet")
    else {
#        print(typeInfo)
        stop("Please make typeInfo$type one of the following: \"hist\", \"ash\", \"dot\", \"spine\", \"dot\"")
    }

#    print(data[brushed == TRUE])
    # This line makes the break values on the continuous variable
    breaks <- calcBinPosition(typeInfo$start, typeInfo$binwidth,
        range(data, na.rm=T)[2], xMaxEndPos(data))
    break_len <- length(breaks)

    if (!is.null(fill)) {
        if (length(unique(fill)) > 1) {
            splitBy <- fill
            message("fill 5 ",length(fill),"\n")

        }
    }
    # This line caclaulates the counts in each bin, and by a second variable
    bar_top <- table(cut(data, breaks = breaks), splitBy)

    # I'm not sure that I need to melt the data?
    data_pos <- reshape::melt(bar_top)
    names(data_pos) <- c("label", "group", "top")
    data_pos$count <- data_pos$top
    data_pos <- data_pos[, c(1, 2, 4, 3)]

    label_names <- unique(data_pos$label)
    group_names <- unique(data_pos$group)

    data_pos$bottom <- rep(0, nrow(data_pos))

    if (is.null(color)) {
        if (length(group_names) == 1) {
            data_pos$color <- rep("grey20", nrow(data_pos))
        }
        else {
#            data_pos$color <- rep(rainbow(length(group_names)),
#                each = length(label_names))
            if (!is.factor(data_pos$group))
                data_pos$group <- factor(data_pos$group)
            data_pos$color <- rep(dscale(data_pos$group, hue_pal()))
        }
    }

    if (position == "dodge") {
        pos <- make_dodge_pos(breaks, length(group_names))
        data_pos$left <- pos$start
        data_pos$right <- pos$end
    }
    else {
        # (position == 'stack' || position == 'relative')
        data_pos$left <- rep(breaks[1:(break_len - 1)], length(group_names))
        data_pos$right <- rep(breaks[2:break_len], length(group_names))

        if (position != "identity") {
            # make the bar_top be stacked (cumulative)
            for (i in 1:nrow(bar_top)) {
                bar_top[i, ] <- cumsum(bar_top[i, ])
            }
            data_pos <- ddply(data_pos, c("label"), transform, top = cumsum(top))
        }

        #make the bar_bottom 'stack'
        data_pos <- data_pos[order(data_pos$top), ]
        data_pos <- ddply(data_pos, "label", transform, bottom = c(0, sort(top)[-length(top)]))

        # relative
        if (position == "relative") {
            data_pos <- ddply(data_pos, c("label"), transform, bottom = divide_by_maximum(bottom,
                top))
            data_pos <- ddply(data_pos, c("label"), transform, top = divide_by_maximum(top))
        }
    }

    # Color Management
    message("fill 1 ",length(fill),"\n")
    f_and_s <- fill_and_stroke(data_pos$color, fill = fill, stroke = stroke)
    message("fill 2 ",length(fill),"\n")
    data_pos$fill <- f_and_s$fill
    data_pos$stroke <- f_and_s$stroke
    data_pos$color <- NULL


    # Brushing
    data_pos$.brushed <- 0
    # data_pos <- ddply(data_pos, c('label', 'group'), transform, .brushed =
    #   percent_of_brushed(left, right, original$data, brushed))
    for (i in seq_len(NROW(data_pos))) {
        data_pos$.brushed[i] <- percent_of_brushed(data_pos[i, "left"], data_pos[i,
            "right"], data, brushed)
    }

    list(data = data_pos, breaks = breaks, label_names = label_names, group_names = group_names)

}


# Make percents pretty
pretty_percent <- function(smallVal) {
    paste(round(100 * smallVal), "%", sep = "")
}


# find a column name unique to the plot
data_hist_column <- function(d) {
    add_unique_column(d, "qhist")
}
data_bin_column <- function(d) {
    add_unique_column(d, "qbin")
}
add_unique_column <- function(d, name) {
    dnames <- names(d)
    locations <- str_detect(dnames, str_c(name, "[1-9]*"))
    if (!any(locations)) {
        return(str_c(name, "1"))
    }
    histNames <- dnames[locations]

    histNumbers <- str_replace_all(histNames, name, "")
    histNumbers <- as.numeric(histNumbers)

    str_c(name, max(histNumbers) + 1)
}

# calculate the break positions including start and end
calcBinPosition <- function(start, width, maxDataPos, maxDrawPos) {
    # cat('start: ', start, '\twidth: ', width, '\tmaxDataPos: ', maxDataPos,
    #   '\tmaxDrawPos: ', maxDrawPos, '\n')
    output <- seq(from = start, to = maxDrawPos, by = width)
    if (max(output) < maxDataPos) {
        output <- c(output, maxDrawPos)
    }
    # cat('calcBinPosition: ', str_c(output, collapse = ', '), '\n')
    output
}

## find the range of the data
#dataRange <- function(d) {
#    range(d)
#}

# find the max binwidth of the data
maxBinwidth <- function(d, na.rm=T) {
    diff(range(d, na.rm=na.rm))/4
}

# execute a base histogram
baseHist <- function(d, ...) {
    output <- suppressWarnings(hist(d, plot = FALSE, ...))
    # print(output)
    output
}

# find the shifting unit when moving the start position
unitShift <- function(d, na.rm=T) {
    diff(range(d, na.rm=na.rm))/60
}

# find the largest deviation from the start position
maxShift <- function(d, na.rm=T) {
    0.1 * diff(range(d, na.rm=na.rm))
}

# find the maximum starting position
xMaxStartPos <- function(d, na.rm=T) {
    range(d, na.rm=na.rm)[1]
}

# find the minimum starting position
xMinStartPos <- function(d, na.rm=T) {
    range(d, na.rm=na.rm)[1] - maxShift(d)
}

# find the maximum ending position
xMaxEndPos <- function(d, na.rm=T) {
    range(d, na.rm=na.rm)[2] + maxBinwidth(d)
}

maxHeight <- function(d, na.rm=T, ...) {
    yMax <- 0
    tmpStartPos <- xMinStartPos(d, na.rm)
    maxStartPos <- xMaxStartPos(d, na.rm)
    while (tmpStartPos <= maxStartPos) {
        tB <- calcBinPosition(tmpStartPos, maxBinwidth(d, na.rm), range(d, na.rm=na.rm)[2], xMaxEndPos(d, na.rm))
        newMax <- max(baseHist(d, breaks = tB, ...)$counts)
        if (newMax > yMax)
            yMax <- newMax
        tmpStartPos <- tmpStartPos + unitShift(d)
    }
    yMax
}
