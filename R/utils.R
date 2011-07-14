extract.formula <- function(form) {
    if (length(form) == 2) {
        formstring <- paste(form[[1]], form[[2]])
    }
    if (length(form) == 3) {
        formstring <- paste(form[[2]], form[[1]], form[[3]])
    }
    return(formstring)

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
make_window_ranges <- function(dataRanges, xlab = NULL, ylab = NULL, xtickmarks = FALSE,
    ytickmarks = FALSE, main = NULL) {

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
    if (xtickmarks) {
        #    ymin = dataRanges[3]-0.05*diff(dataRanges[3:4])
        ymin = ymin - 0.05 * diff(dataRanges[3:4])
    }

    # based on length of y tickmarks extra space
    if (ytickmarks) {
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


##' Extend the range of data by an amount.
##'
##' This is useful for setting a margin in the plot region.
##'
##' @param x the data vector (either the orginal vector or its range)
##' or a 2x2 matrix which defines the ranges of two axes in two
##' columns
##' @param f the amount to extend the range
##' @return a vector or a matrix of ranges corresponding to the input
##' \code{x}
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples extend_ranges(c(0, 1))
##' extend_ranges(1:10)
##'
##' m = matrix(c(c(1,10), c(5, 8)), nrow = 2)
##' extend_ranges(m)
##' extend_ranges(m, c(.1, .2))
##'
extend_ranges = function(x, f = qpar("mar")) {
    if (is.matrix(x)) {
        if (!identical(dim(x), c(2L, 2L)))
            stop("the range matrix must be of dim 2x2")
        x = apply(x, 2, sort)
        return(x + f * t((x[2, ] - x[1, ]) %*% t(c(-1, 1))))
    }
    x = range(x, na.rm = TRUE)
    x + c(-1, 1) * f * (x[2] - x[1])
}

##' Re-order the columns of a data frame based on MDS or ANOVA.
##'
##' For the MDS method, we use (1 - correlation matrix) as the
##' distance matrix and re-order the columns according their distances
##' between each other (i.e. 1-dimension representation of
##' p-dimension); as a result, columns in a neighborhood indicate they
##' are more similar to each other. For the ANOVA method, if there is
##' a column named \code{.color}, it will be used as the x variable
##' and we perform ANOVA on each numeric column vs this variable, then
##' the columns are re-ordered by the P-values, so the colors can
##' discriminate the first few columns most apart. Of course, when
##' there is only a single color in the \code{.color} variable, the
##' ANOVA method will not work and the original order will be
##' returned. For the randomForest method, the variables will be
##' ordered by the importance scores (mean descrease in accuracy) and
##' the argument \code{x} will be used as the response variable.
##' @param data a data frame (or similar data structures like mutaframes)
##' @param type the method to re-order the variables (columns)
##' @param vars the column names of the \code{data}
##' @param numcol a logical vector indicating which columns are numeric
##' @param x the x variable to be used in ANOVA and randomForest
##' @return the column names (i.e. the argument \code{vars}) after
##' being re-ordered; note non-numeric variables will always be put in
##' the end and they will not go into the computation
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' data(tennis)
##' reorder_var(tennis, type = 'MDS')
##'
##' reorder_var(iris, type = 'ANOVA', x = iris$Species)
##' names(iris)  # original column names
##' reorder_var(iris, type = 'randomForest', x = iris$Species)
##'
reorder_var = function(data, type = c('none', 'MDS', 'ANOVA', 'randomForest'),
                       vars = names(data), numcol = sapply(data, is.numeric),
                       x = data$.color) {
    type = match.arg(type)
    if (any(numcol)) {
        num_data = data[, numcol, drop = FALSE]
        switch(type, none = NULL, MDS = {
            idx = order(cmdscale(1 - cor(num_data), k = 1))
        }, ANOVA = {
            if (!is.null(x) && length(unique(x)) > 1) {
                xfactor = factor(x)
                idx = order(apply(num_data, 2, function(y) {
                    summary(aov(y ~ xfactor))[[1]][1, 5]
                }))
            } else {
                idx = 1:ncol(num_data)
            }
        }, randomForest = {
            if (!is.null(x) && length(unique(x)) > 1 && require('randomForest')) {
                xfactor = factor(x)
                imp = randomForest(num_data, xfactor, importance = TRUE)$importance
                idx = order(-imp[, ncol(imp) - 1])
            } else {
                idx = 1:ncol(num_data)
            }
        })
        if (type != 'none') {
            return(c(vars[numcol][idx], vars[!numcol]))
        }
    }
    vars
}

##' Insert line breaks into character strings.
##' By default, all the non-alphanumeric characters are replaced by
##' \code{'\n'}, which can be useful when plotting long axis labels,
##' e.g., in parallel coordinates plots.
##'
##' @param x a character vector
##' @param split the rule (regular expression) to replace characters by line breaks
##' @param ... other arguments passed to \code{\link[base]{gsub}}
##' @return a character vector with certain characters replaced by \code{'\n'}
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples
##' break_str(c('long label1.1', 'long label1.2', 'long label1.3'), split = ' ')
##' break_str(names(iris))
##'
break_str = function(x, split = '[^[:alnum:]]', ...) {
    gsub(split, '\n', x, ...)
}

##' Match keys from a keyboard event.
##' This is a simple wrapper function to test if the given keys are
##' hit in the keyboard event.
##'
##' @param key a character vector of key names (see the example below)
##' @param event the keyboard event
##' @param logical whether to return a logical vector or an integer
##' vector (the latter applies \code{\link[base]{which}} to the
##' logical vector)
##' @return \code{TRUE} for the matched keys, and \code{FALSE} for
##' those not matched; or an integer vector of the indices of the
##' matched keys
##' @author Yihui Xie <\url{http://yihui.name}>
##' @references \url{http://doc.qt.nokia.com/latest/qt.html#Key-enum}
##' @export
##' @examples library(qtbase)
##' library(qtpaint)
##' library(cranvas)
##' key_press = function(layer, event) {
##' print(match_key(c('A', 'F', 'PageUp', '1'), event))
##' }
##' s = qscene(); r = qlayer(s, keyPressFun = key_press)
##' qplotView(scene = s)
match_key = function(key, event, logical = TRUE) {
    k = event$key()
    e = attr(Qt$Qt, 'env')
    res = sapply(key, function(x) e[[sprintf('Key_%s', x)]] == k, USE.NAMES = FALSE)
    if (logical) res else which(res)
}
