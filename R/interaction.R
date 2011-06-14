##' Create a mutaframe from data with attributes for interaction.
##'
##' This function will first check if the names of some predefined row
##' attributes (e.g. \code{.color}, \code{.brushed}) exist in the
##' column names of the data (will issue an error if they do); then
##' append these columns to the original data to create an augmented
##' data as a \code{\link[plumbr]{mutaframe}}; in the end add some
##' attributes to the mutaframe to control the behavior of interaction
##' (mainly the \code{\link{brush}} object and the
##' \code{\link{link}}ing specification).
##'
##' @param data a data frame (typically); it will be coerced to a data
##' frame
##' @param color colors of graphical elements (default black)
##' corresponding to rows of data
##' @param fill colors for filling the graphical elements
##' (e.g. rectangles)
##' @param size sizes of rows (default 1)
##' @param brushed a logical vector indicating whether the rows are
##' brushed (default all \code{FALSE})
##' @param visible a logical vector indicating whether the rows are
##' visible (default all \code{TRUE})
##' @return a mutaframe with attributes for interaction
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link[plumbr]{mutaframe}}, \code{\link{brush}},
##' \code{\link{link}}
##' @export
##' @examples
##' iris0 = qdata(iris, color = 'red', brushed = FALSE)
##' ## thicker line for brushed elements
##' brush(iris0, 'size') = 3
##' qparallel(iris0)
##' ## random colors
##' iris0$.color = sample(1:8, nrow(iris), replace = TRUE)
##' ## change the colors to green
##' iris0$.color = 'green'
##' ## 'brushing' by command line
##' for (i in 1:10) {
##'     selected(iris0) = sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)
##'     Sys.sleep(1)
##' }
##' ## change the brush color to green
##' brush(iris0, 'color') = 'green'
##' ## change brushed lines to black
##' brush(iris0, 'color') = 'black'
qdata = function(data, color = "black", fill = "grey30", size = 1, brushed = FALSE, visible = TRUE) {
    if (!is.data.frame(data))
        data = as.data.frame(data)
    ## check if the attribute exists
    ## row attributes needed by all plotting functions
    row_attrs = c(".color", ".fill", ".size", ".brushed", ".visible")
    ## once in a blue moon...
    conflict_attrs = row_attrs %in% colnames(data)
    if (any(conflict_attrs)) {
        stop(sprintf("variable names conflicts: %s already exist(s) in data", paste(row_attrs[conflict_attrs],
            collapse = ", ")))
    }
    mf = data
    ## initialize here; TODO: get rid of this in qparallel, qmosaic...
    mf$.brushed = brushed
    mf$.color = color
    mf$.fill = fill
    mf$.size = size
    mf$.visible = TRUE

    ## shadow matrix for missing values
    shadowmatrix = is.na(data)
    colnames(shadowmatrix) = paste('NA', colnames(data), sep='.')

    ## add shadow matrix at the end if there are missing values
    if (sum(shadowmatrix)) {
        idx = (colSums(shadowmatrix) > 0)
        mf = data.frame(mf,shadowmatrix[,idx])
        warning(paste('Missing values: there are',sum(shadowmatrix),'missing values in',sum(idx),'columns.'))
    }

    ## prevent converting from characters to factors
    if (!is.mutaframe(mf)) {
        old_opts = options(stringsAsFactors = FALSE)
        mf = as.mutaframe(mf)
        on.exit(options(old_opts))
    }


    ## attach a brush to this data; we need to create the xxxChanged event in
    #   specific plots
    ## use brush(data) to access this brush
    attr(mf, "Brush") = brushGen$new(style = list(color = "yellow", size = 2, linetype = NULL),
        color = "yellow", color.gen = function(...) NULL, size = 2, size.gen = function(...) NULL,
        mode = "none", identify = FALSE, label.gen = function(...) {
            paste(capture.output(print(...)), collapse = '\n')
        }, label.color = "darkgray",
        history.size = 30, history.index = 0, history.list = list(),
        persistent = FALSE, persistent.color = character(0), persistent.list = list(),
        cursor = 0L)

    ## here 'mode' is explained in the documentation of mode_selection()

    ## specifies which variable is used for (hot/cold) linking
    ## use link_var(data) to access the linking variable
    attr(mf, "Link") = mutalist(linkvar = NULL, type = "hot", focused = FALSE)

    ## and other possible attributes

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
##' @param x logical: the previous selection status
##' @param y logical: the current selection status
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
mode_selection = function(x, y, mode = "none") {
    ## a series of logical operations
    ## if mode is not specified, return y, the current status
    switch(mode, none = y, and = x & y, or = x | y, xor = xor(x, y), not = x & !y,
        complement = !y, y)
}


##' Set or query the focus status.
##'
##' The plot on top of all the rest of plots is on focus, and the
##' corresponding mutaframe is said to be on focus too.
##' @param data the mutaframe
##' @return a logical value: whether the plot corresponding to this
##' mutaframe is on focus or not
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
##' @return \code{NULL}; the status of focus is changed to \code{value}
`focused<-` = function(data, value) {
    attr(data, "Link")[["focused"]] = value
    data
}

##' Set or query the visibility of observations.
##'
##' There is a column \code{.visible} to control the visibility of
##' observations. This can be useful for ``deleting'' certain
##' observations from the plot (set their visibility to \code{FALSE}).
##' @param data the mutaframe
##' @return returns the logical vector to control the visibility of
##' observations
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
##' @return changes the logical vector of visibility (i.e., \code{data$.visible})
`visible<-` = function(data, value) {
    data$.visible = value
    data
}

##' Set or query the selected (brushed) observations.
##'
##' The column \code{.brushed} controls which observations are being
##' brushed (i.e. those \code{TRUE}'s are selected).
##' @param data the mutaframe
##' @return returns the logical vector corresponding to whether the
##' observations are selected or not
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{qdata}}
##' @export
##' @examples df = qdata(iris)
##'
##' selected(df)
##'
##' selected(df) = rep(c(TRUE, FALSE), c(10, 140))  # brush the first 10 obs
##'
##' selected(df)
##'
selected = function(data) {
    if ('.brushed' %in% names(data))
        data$.brushed else logical(nrow(data))
}
##' @rdname selected
##' @usage selected(data) <- value
##' @param value a logical vector of the length \code{nrow(data)}
##' @export "selected<-"
##' @return sets the selected status of observations
`selected<-` = function(data, value) {
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

##' Set the cursor of a view.
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
##' @return NULL; the cursor of the view is set as a side effect
##' @author Yihui Xie <\url{http://yihui.name}>
##' @references \url{http://doc.qt.nokia.com/4.7/qt.html#CursorShape-enum}
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
