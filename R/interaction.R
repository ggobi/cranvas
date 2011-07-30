##' Create a mutaframe from data with attributes for interaction
##'
##' This function will first check if the names of some pre-defined
##' row attributes (e.g. \code{.color}, \code{.brushed}) exist in the
##' column names of the data (will issue an error if they do); then
##' append these columns to the original data to create an augmented
##' data as a \code{\link[plumbr]{mutaframe}}; in the end add some
##' attributes to the mutaframe for the purpose of interaction (mainly
##' the \code{\link{brush}} object and the \code{\link{link}}ing
##' specification). A shadow matrix will be attached if any missing
##' values are present in the original data.
##'
##' When the three arguments \code{color}, \code{fill} and \code{size}
##' take values as variable names in \code{data}, default palettes
##' will be used to generate colors and sizes. The sequential color
##' gradient palette (\code{\link[scales]{seq_gradient_pal}}) will be
##' applied to continuous variables, and the hue palette
##' (\code{\link[scales]{hue_pal}}) will be applied to categorical
##' variables. The area palette (\code{\link[scales]{area_pal}}) is
##' used to create a size vector when the size variable is
##' continuous. If any palette is used, an attribute \code{attr(data,
##' 'Scales')} will be attached to the returned mutaframe, which will
##' help specific plots to generate legends.
##'
##' @param data a data frame (it will be coerced to a data frame if it
##' is not)
##' @param color colors of graphical elements (default black)
##' corresponding to rows of data; it can be a vector of valid R
##' colors, or a name of variable in \code{data} (must be either a
##' factor or a numeric variable), or an R expression to calculate
##' colors
##' @param fill colors for filling the graphical elements
##' (e.g. rectangles); possible values are similar to \code{color}
##' @param size sizes of rows (default 1); possible values are similar
##' to \code{color}, but when using a variable to generate sizes, it
##' must be a numeric variable
##' @param brushed a logical vector indicating whether the rows are
##' brushed (default all \code{FALSE})
##' @param visible a logical vector indicating whether the rows are
##' visible (default all \code{TRUE})
##' @return a mutaframe with attributes for interaction
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link[plumbr]{mutaframe}}, \code{\link{brush}},
##' \code{\link{link}}
##' @export
##' @example inst/examples/qdata-ex.R
qdata = function(data, color = "black", fill = "grey30", size = 1, brushed = FALSE, visible = TRUE) {
    if (!is.data.frame(data))
        data = as.data.frame(data)
    ## check if the attribute exists
    ## row attributes needed by all plotting functions
    row_attrs = c(".color", ".fill", ".size", ".brushed", ".visible")
    ## once in a blue moon...
    conflict_attrs = row_attrs %in% colnames(data)
    if (any(conflict_attrs)) {
        stop(sprintf("variable names conflicts: %s already exist(s) in data",
                     paste(row_attrs[conflict_attrs], collapse = ", ")))
    }
    mf = data
    mf$.brushed = brushed
    mf$.visible = visible

    z = as.list(match.call()[-1])
    l = list()
    for (i in c('color', 'fill', 'size')) {
        if (is.language(z[[i]])) {
            data(munsell_map, package = "munsell")
            v = eval(z[[i]], data)
            mf[[sprintf('.%s', i)]] = if (i != 'size') {
                if (is.factor(v)) dscale(v, hue_pal()) else if (is.numeric(v))
                    cscale(v, seq_gradient_pal()) else
                if (!inherits(try(col2rgb(v), silent = TRUE), 'try-error')) v else
                    stop(i, ' must be either a factor or a numeric variable or valid colors!')
            } else if (is.numeric(v)) area_pal()(v) else
                stop('size must be numeric!')
            l[[i]] = deparse(z[[i]])
        } else mf[[sprintf('.%s', i)]] = switch(i, color = color, fill = fill, size = size)
    }

    ## prevent converting from characters to factors
    if (!is.mutaframe(mf)) {
        old_opts = options(stringsAsFactors = FALSE)
        mf = as.mutaframe(mf)
        on.exit(options(old_opts))
    }

    ## attach a brush to data; we need to create the xxxChanged event in specific plots
    ## use brush(data) to access this brush
    attr(mf, "Brush") =
        brushGen$new(style = list(color = "yellow", linewidth = 2, linetype = NULL),
                     color = "yellow", color.gen = function(...) NULL,
                     size = 2, size.gen = function(...) NULL,
                     mode = "none", identify = FALSE, label.gen = function(...) {
                         paste(capture.output(print(...)), collapse = '\n')
                     }, label.color = "darkgray",
                     history.size = 30, history.index = 0, history.list = list(),
                     persistent = FALSE, persistent.color = character(0),
                     persistent.list = list(), cursor = 0L)

    ## here 'mode' is explained in the documentation of mode_selection()

    ## specifies which variable is used for (hot/cold) linking
    ## use link_var(data) to access the linking variable
    attr(mf, "Link") = mutalist(linkvar = NULL, type = "hot", focused = FALSE)

    shadow = is.na(data)  # shadow matrix for missing values
    ## add shadow matrix to 'shadow' attribute
    if (sum(shadow)) {
        idx = (colSums(shadow) > 0)
        message('There are ', sum(shadow),' missing values in ', sum(idx), ' columns.',
                ' A shadow matrix is attached to attr(data, "Shadow")')
        attr(mf, 'Shadow') = shadow
        add_listener(mf, function(i, j) {
            if (!(j %in% row_attrs))
                attr(mf, 'Shadow') = is.na(as.data.frame(mf[, !(names(mf) %in% row_attrs)]))
        })  # shadow matrix will change when data is changed
    }

    attr(mf, 'Scales') = l  # scales information to be used in legend
    mf
}


##' Logical operations under different selection mode
##'
##' A selection mode is essentially a logical operation like AND, OR, and XOR, etc.
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
##' @param x logical: the previous selection status
##' @param y logical: the current selection status (if \code{y} is a
##' numeric vector, it will be converted to a logical vector of the
##' same length with \code{x} with \code{TRUE}'s corresponding to the
##' numeric indicies)
##' @param mode the selection mode string; see Details
##' @return a logical vector indicating whether the objects are selected
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link[base]{&}}, \code{\link[base]{|}},
##' \code{\link[base]{xor}}, \code{\link[base]{!}}
##' @export
##' @examples
##' x1 = c(TRUE, TRUE, FALSE, FALSE)
##' x2 = c(FALSE, TRUE, TRUE, FALSE)
##' mode_selection(x1, x2, 'none')
##' mode_selection(x1, x2, 'and')
##' mode_selection(x1, x2, 'or')
##' mode_selection(x1, x2, 'xor')
##' mode_selection(x1, x2, 'not')
##' mode_selection(x1, x2, 'complement')
##'
##' mode_selection(x1, c(2, 3), 'and')  # equivalent to x2
mode_selection = function(x, y, mode = "none") {
    if (is.numeric(y)) {
        tmp = logical(length(x))
        tmp[y] = TRUE
        y = tmp
    }
    ## a series of logical operations
    ## if mode is not specified, return y, the current status
    switch(mode, none = y, and = x & y, or = x | y, xor = xor(x, y), not = x & !y,
        complement = !y, y)
}


##' Set or query the focus status
##'
##' The plot on top of all the rest of plots is on focus, and the
##' corresponding mutaframe is said to be on focus too.
##' @param data the mutaframe
##' @return The function \code{\link{focused}} returns a logical
##' value: whether the plot corresponding to \code{data} is on focus
##' or not
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{qdata}}
##' @export
##' @examples
##' mf = qdata(head(iris))
##'
##' focused(mf)
##'
##' focused(mf) = TRUE
##'
focused = function(data) {
    attr(data, "Link")[["focused"]]
}

##' @rdname focused
##' @usage focused(data) <- value
##' @param value a logical value: whether on focus or not
##' @export "focused<-"
`focused<-` = function(data, value) {
    attr(data, "Link")[["focused"]] = value
    data
}

##' Set or query the visibility of observations
##'
##' There is a column \code{.visible} to control the visibility of
##' observations. This can be useful for ``deleting'' certain
##' observations from the plot (set their visibility to \code{FALSE}).
##' @param data the mutaframe
##' @return The function \code{\link{visible}} returns the logical
##' vector to control the visibility of observations
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{qdata}}
##' @export
##' @examples df = qdata(iris)
##'
##' visible(df)
##'
##' visible(df) = rep(c(TRUE, FALSE), c(100, 50))  # hide the last 50 obs
##'
##' visible(df)
##'
visible = function(data) {
    data$.visible
}
##' @rdname visible
##' @usage visible(data) <- value
##' @param value a logical vector of the length \code{nrow(data)}
##' @export "visible<-"
`visible<-` = function(data, value) {
    data$.visible = value
    data
}

##' Set or query the selected (brushed) observations
##'
##' The column \code{.brushed} controls which observations are being
##' brushed (i.e. those \code{TRUE}'s are selected).
##' @param data the mutaframe
##' @return The function \code{\link{selected}} returns the logical
##' vector corresponding to whether the observations are selected or
##' not
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{qdata}}
##' @export
##' @examples df = qdata(iris)
##'
##' selected(df)
##'
##' selected(df) = rep(c(TRUE, FALSE), c(10, 140))  # brush the first 10 obs
##' selected(df)
##'
##' selected(df) = 3L  # brush the 3rd row
##' selected(df)
selected = function(data) {
    if ('.brushed' %in% names(data))
        data$.brushed else logical(nrow(data))
}
##' @rdname selected
##' @usage selected(data) <- value
##' @param value a logical vector of the length \code{nrow(data)}, or
##' a vector of integers which will be used to create a logical vector
##' with \code{TRUE} corresponding to these indicies
##' @export "selected<-"
`selected<-` = function(data, value) {
    ## if value is numeric indices, convert it to a logical vector
    if (is.numeric(value)) {
        tmp = logical(nrow(data))
        tmp[value] = TRUE
        value = tmp
    }
    data$.brushed = value
    data
}

.QtCursor = structure(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
    9L, 10L, 11L, 12L, 13L, 14L, 17L, 18L, 15L, 16L, 20L,
    19L, 21L, 24L), .Names = c("ArrowCursor", "UpArrowCursor",
    "CrossCursor", "WaitCursor", "IBeamCursor", "SizeVerCursor",
    "SizeHorCursor", "SizeBDiagCursor", "SizeFDiagCursor",
    "SizeAllCursor", "BlankCursor", "SplitVCursor", "SplitHCursor",
    "PointingHandCursor", "ForbiddenCursor", "OpenHandCursor",
    "ClosedHandCursor", "WhatsThisCursor", "BusyCursor",
    "DragMoveCursor", "DragCopyCursor", "DragLinkCursor",
    "BitmapCursor"))

##' Set the cursor of a view
##'
##' Change the shape of cursor on a view.
##'
##' All possible cursor types with the corresponding integer code are:
##'
##' 0: ArrowCursor; 1: UpArrowCursor; 2: CrossCursor; 3: WaitCursor;
##' 4: IBeamCursor; 5: SizeVerCursor; 6: SizeHorCursor; 7:
##' SizeBDiagCursor; 8: SizeFDiagCursor; 9: SizeAllCursor; 10:
##' BlankCursor; 11: SplitVCursor; 12: SplitHCursor; 13:
##' PointingHandCursor; 14: ForbiddenCursor; 17: OpenHandCursor; 18:
##' ClosedHandCursor; 15: WhatsThisCursor; 16: BusyCursor; 20:
##' DragMoveCursor; 19: DragCopyCursor; 21: DragLinkCursor; 24:
##' BitmapCursor
##'
##' We can pass either the integer code or the character string to the
##' \code{cursor} argument.
##' @param view the view for which to change the cursor (created by
##' \code{\link[qtpaint]{qplotView}})
##' @param cursor an integer or a character string (see Details)
##' @return \code{NULL}; the cursor of the view is set as a side
##' effect
##' @author Yihui Xie <\url{http://yihui.name}>
##' @references \url{http://doc.qt.nokia.com/latest/qt.html#CursorShape-enum}
##' @export
##' @examples
##' library(cranvas)
##' library(qtpaint)
##' scene = qscene()
##' qlayer(scene)
##' v = qplotView(scene = scene)
##' print(v)
##'
##' set_cursor(v, 'WaitCursor')
##'
##' set_cursor(v, 2L)  # CrossCursor
##'
set_cursor = function(view, cursor = 'ArrowCursor') {
    cu = view$cursor
    if (is.character(cursor)) cursor = .QtCursor[cursor]
    cu$setShape(cursor)
    view$cursor = cu
}
