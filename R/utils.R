extract.formula <- function(form) {
    if (length(form) == 2) {
        formstring <- paste(form[[1]], form[[2]])
    }
    if (length(form) == 3) {
        formstring <- paste(form[[2]], form[[1]], form[[3]])
    }
    return(formstring)

}

find_id <- function(var) {
    if (!(length(levels(var)) == 0)) {
        xid <- levels(var)
    }
    else {
        if (is.numeric(var) || is.integer(var))
            xid <- pretty(var)
    }
    return(xid)
}

find_xid <- function(data, colName) {
    cols <- subset(data, select = colName)[, 1]
    if (!(length(levels(cols[1])) == 0)) {
        xid <- levels(cols[1])
    }
    else if (class(cols[1]) == "numeric" || class(cols[1]) == "integer") {
        xid <- pretty(cols)
    }
    return(xid)
}

find_yid <- function(data, colName) {
    cols <- subset(data, select = colName)[, 1]
    if (!(length(levels(cols[1])) == 0)) {
        yid <- levels(cols[1])
    }
    else if (class(cols[1]) == "numeric" || class(cols[1]) == "integer")
        yid <- pretty(cols)
    else stop("data type not supported")


    return(yid)
}

get_axisPos <- function(var) {
    if (!(length(levels(var[1])) == 0)) {
        by <- 1/(length(levels(var[1])) + 1)
        majorPos <- seq.int(c(0:1), by = by)
    }
    else {
        if (is.numeric(var) || is.integer(var))
            majorPos <- pretty(var)
        else stop("data type not supported")
    }

    return(majorPos)
}


get_axisPosX <- function(data, colName) {
    cols <- subset(data, select = colName)[, 1]
    if (!(length(levels(cols[1])) == 0)) {
        by <- 1/(length(levels(cols[1])) + 1)
        majorPos <- seq.int(c(0:1), by = by)
    }
    else if (class(cols[1]) == "numeric" || class(cols[1]) == "integer") {
        majorPos <- pretty(cols)
    }
    else {
        stop("data type not supported")
    }

    return(majorPos)
}

get_axisPosY <- function(data, colName) {
    cols <- subset(data, select = colName)[, 1]
    if (!(length(levels(cols[1])) == 0)) {
        by <- 1/(length(levels(cols[1])) + 1)
        majorPos <- seq.int(c(0:1), by = by)
    }
    else if (class(cols[1]) == "numeric" || class(cols[1]) == "integer") {
        majorPos <- pretty(cols)
    }
    else {
        stop("data type not supported")
    }

    return(majorPos)
}

find_x_label <- function(df) {
    vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

    axis.set <- subset(df, (b == min(b)) & (level == max(level)))

    paste(vars[sapply(vars, function(x) return(length(unique(axis.set[, x])) > 1))],
        "")
}

find_y_label <- function(df) {
    vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

    axis.set <- subset(df, (l == min(l)) & (level == max(level)))

    paste(vars[sapply(vars, function(x) return(length(unique(axis.set[, x])) > 1))],
        "")
}


#' makes the data ranges
#' makes the data ranges with a small buffer
#'
#' @param dataColumn vector containing numeric elements
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  make_data_ranges(mtcars$disp)
make_data_ranges <- function(dataColumn) {
    maxdata <- max(dataColumn, na.rm = T)
    mindata <- min(dataColumn, na.rm = T)
    range <- maxdata - mindata

    # add a five percent space around all points
    c(mindata - 0.05 * range, maxdata + 0.05 * range)
}


#' make the window ranges
#' make the window ranges
#'
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @param xlab if xlab is replaced with somthing other than null, it will be assumed that an axis label will be used
#' @param ylab if ylab is replaced with somthing other than null, it will be assumed that an axis label will be used
#' @return returns a vector of four variables containing xmin, xmax, ymin, ymax
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  make_window_ranges(c(0,1,2,3))
make_window_ranges <- function(dataRanges, xlab = NULL, ylab = NULL, xtickmarks = NULL,
    ytickmarks = NULL, main = NULL) {

    # add more space for the Y label
    if (!is.null(ylab)) {
        xmin = dataRanges[1] - 0.1 * diff(dataRanges[1:2])
    }
    else {
        xmin = dataRanges[1] - 0.065 * diff(dataRanges[1:2])
    }
    xmax = dataRanges[2] + 0.05 * diff(dataRanges[1:2])


    # add more space for the X label
    if (!is.null(xlab)) {
        ymin = dataRanges[3] - 0.1 * diff(dataRanges[3:4])
    }
    else {
        ymin = dataRanges[3] - 0.065 * diff(dataRanges[3:4])
    }
    ymax = dataRanges[4] + 0.05 * diff(dataRanges[3:4])

    # little extra space necessary for xtickmarks
    if (!is.null(xtickmarks)) {
        #    ymin = dataRanges[3]-0.05*diff(dataRanges[3:4])
        ymin = ymin - 0.05 * diff(dataRanges[3:4])
    }

    # based on length of y tickmarks extra space
    if (!is.null(ytickmarks)) {
        #xwidth = max(str_length(as.character(ytickmarks)))
        # each character gives 0.75% extra space
        #    xmin = dataRanges[1] - 0.0075*xwidth*diff(dataRanges[1:2])
        xmin = xmin - 0.0075 * diff(dataRanges[1:2])
    }

    # extra space for window title
    if (!is.null(main)) {
        if (length(main) > 0)
            ymax = ymax + 0.05 * diff(dataRanges[3:4])
    }

    windowRanges <- c(xmin, xmax, ymin, ymax)

    windowRanges
}


##' Extend the range of data by an amount \code{qpar('mar')}.
##'
##' This is useful to set a margin in the plot region.
##'
##' @param x the data vector (either the orginal vector or its range)
##' or a matrix of 2x2 which defines the ranges of two axes in two
##' columns
##' @param f the amount to extend the range
##' @return a vector of length 2: the lower and upper bounds
##' @author Yihui Xie <\url{http://yihui.name}>
##' @nord
##' @examples .extend.ranges(c(0, 1))
##' .extend.ranges(1:10)
##' .extend.ranges(matrix(c(c(1,10), c(5, 8)), nrow = 2))
##'
.extend.ranges = function(x, f = qpar("mar")) {
    if (is.matrix(x)) {
        if (!identical(dim(x), c(2L, 2L)))
            stop("the range matrix must be of dim 2x2")
        x = apply(x, 2, sort)
        return(x + f * t((x[2, ] - x[1, ]) %*% t(c(-1, 1))))
    }
    x = range(x)
    x + c(-1, 1) * f * (x[2] - x[1])
}
