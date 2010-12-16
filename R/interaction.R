##' Create a mutaframe from data with several attributes for interaction
##'
##' Create a mutaframe from data with several attributes for interaction:
##' first check if the names of some predefined row attributes (e.g. .color, .brushed)
##' exist in the data (will issue an error if this happens); then augment the ...
##' arguments to the data and convert the augmented data to a mutaframe; in the end
##' add some attributes to the mutaframe to control the appearance of elements for
##' interaction (e.g. the color of the brush, the size of the brushed objects, and
##' whether to show the labels of the brushed objects).
##' @title Create a Mutaframe from Data with Several Attributes for Interaction
##' @param data a data frame (typically); it will be coerced to a data
##' frame
##' @param ... other attributes corresponding to rows such as colors,
##' sizes and so on
##' @return a mutaframe
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' iris0 = qmutaframe(iris, .color = 'red', .brushed = FALSE)
##' ## the line width >= 2 does not work for me, so use 1
##' brush_attr(iris0, '.brushed.size') = 1
##' qparallel(iris0)
##' ## random colors
##' iris0$.color = sample(1:8, nrow(iris), replace = TRUE)
##' ## change the colors to green
##' iris0$.color = 'green'
##' ## 'brushing' by command line
##' for (i in 1:10) {
##'     iris0$.brushed = sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)
##'     Sys.sleep(1)
##' }
##' ## change the brush color to green
##' brush_attr(iris0, '.brush.color') = 'green'
##' ## change brushed lines to black
##' brush_attr(iris0, '.brushed.color') = 'black'
qmutaframe = function(data, ...) {
    if (!is.data.frame(data)) data = as.data.frame(data)
    ## check if the attribute exists
    ## row attributes needed by all plotting functions
    row_attrs=c('.color', '.size', '.brushed')
    ## once in a blue moon...
    conflict_attrs = row_attrs %in% colnames(data)
    if(any(conflict_attrs)) {
        stop(sprintf('variable names conflicts: %s already exist(s) in data',
                     paste(row_attrs[conflict_attrs], collapse = ', ')))
    }

    ## prevent converting from characters to factors
    old_opts = options(stringsAsFactors = FALSE)
    mf = as.mutaframe(data.frame(data, ...))

    ## we need to store some attributes somewhere which are not corresponding to rows
    ## e.g. attrs related to the brush (scalars, functions, or data frames)
    attr(mf, '.brush.attr') = mutalist(.brush.color = 'yellow', .brush.size = 1,
        .brushed.color = 'yellow', .brushed.size = 2, .brush.mode = 'none',
        .label.show = FALSE, .label.fun = summary_one, .label.color = 'gray40',
        .brush.history = list(), .brush.index = 0, .history.size = 30)
    ## here '.brush.mode' is explained in the documentation of mode_selection()

    ## and other possible attributes

    options(old_opts)

    mf
}


##' Logical operations under different selection mode.
##'
##' There are five selection modes:
##' \describe{
##'   \item{none}{ignore previous selection and completely start over again}
##'   \item{and}{select the intersection, i.e. the objects that are selected by two successive brushing operations}
##'   \item{or}{select the union, i.e. any objects selected by all previous operations and the current operation}
##'   \item{xor}{toggle the selection}
##'   \item{not}{negation, i.e. exclude the objects under two successive brushing operations}
##'   \item{complement}{the complement of the current selection}
##' }
##' We can hold the key while brushing: A for 'and', O for 'or', X for 'xor', N for 'not' and C for 'complement'.
##' @title Logical Operations Under Different Selection Mode
##' @param x logical: the previous selection status
##' @param y logical: the current selection status
##' @param mode the selection mode string; see Details
##' @return a logical vector indicating whether the objects are selected
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' x1 = c(TRUE, TRUE, FALSE, FALSE)
##' x2 = c(FALSE, TRUE, TRUE, FALSE)
##' mode_selection(x1, x2, 'none')
##' mode_selection(x1, x2, 'and')
##' mode_selection(x1, x2, 'or')
##' mode_selection(x1, x2, 'xor')
##' mode_selection(x1, x2, 'not')
##' mode_selection(x1, x2, 'complement')
mode_selection = function(x, y, mode = 'none'){
    ## a series of logical operations
    ## if mode is not specified, return y, the current status
    switch(mode, none = y, and = x & y, or = x | y, xor = xor(x, y), not = x & !y,
           complement = !y, y)
}



##' Get or set brush attributes
##'
##' @aliases brush_attr brush_attr<-
##' @usage brush_attr(data, attr)
##' brush_attr(data, attr) <- value
##' @param data the mutaframe created by \code{\link{qmutaframe}},
##' with an attribute '.brush.attr'
##' @param attr the name of the brush attribute, e.g. '.brush.color'
##' (the color of the brush), '.brushed.color' (the color of the
##' objects selected by the brush), '.brush.size' (the line width of
##' the brush), and '.brushed.size' (the size of the selected objects,
##' e.g. line width or size of points); \code{attr} can be a vector to
##' access multiple attributes when querying brush attributes (but it
##' is only allowed to set one attribute at a time)
##' @return the brush attribute(s) (or as a side effect, change the attribute of
##' \code{data})
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples qiris = qmutaframe(head(iris))
##' brush_attr(qiris)  # all attributes
##' brush_attr(qiris, '.brush.color')
##' brush_attr(qiris, c('.brushed.color', '.brushed.size'))
##' brush_attr(qiris, '.brush.color') = 'green'  # set brush color to green
brush_attr = function(data, attr) {
    .brush.attr = base::attr(data, '.brush.attr')
    if (missing(attr)) {
        .brush.attr
    } else {
        if (length(attr) == 1) .brush.attr[[attr]] else .brush.attr[attr]
    }
}
`brush_attr<-` = function(data, attr, value) {
    attr(data, '.brush.attr')[[attr]] = value
    data
}



##' Truncate Strings
##'
##' Truncate a string if its length is greater than a specified \code{length},
##' with an extra flag appended in the end.
##' @title Truncate Strings to the Specified Length
##' @param x a character vector
##' @param length the desired length
##' @param extra the characters to be appended to the strings if their lengths are greater than the specified \code{length}
##' @return the truncated strings
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' truncate_str('asdfasdf', 5)
##' truncate_str(c('asdf', 'qwer'), 2)
##' truncate_str(c('asdf', 'qwer'), c(1, 4))
##' truncate_str(c('asdf', 'qwer'), 3, '??')
truncate_str = function(x, length, extra = '...') {
    paste(substr(x, 1, length), ifelse(nchar(x) > length, extra, ''), sep = '')
}


qclose <- function(x) {
	x$close()
}
