## template to generate a brush
brushGen = setRefClass('BRUSH',
              fields = c(signalingField('style', 'list'), # style: color, size, linetype
              signalingField('color', 'character'), # color of brushed elements
              signalingField('color.gen', 'function'), # fun to generate color
              signalingField('size', 'numeric'), # size of brushed elements
              signalingField('size.gen', 'function'), # fun to generate size
              signalingField('mode', 'character'), # brush mode
              signalingField('identify', 'logical'), # whether to identify
              signalingField('label.gen', 'function'), # fun to return texts
              signalingField('label.color', 'character'), # col of texts
              signalingField('history.size', 'numeric'), # history to store
              signalingField('history.index', 'numeric'), # current index
              signalingField('history.list', 'list'), # brush history
              signalingField('persistent', 'logical'), # persistent or transient brushing
              signalingField('persistent.color', 'character'), # persistent colors
              signalingField('persistent.list', 'list'), # persistent brushing history
              signalingField('cursor', 'numeric') # the cursor type
              ))


##' Set or query the brush attributes
##'
##' The brush object in \pkg{cranvas} is essentially an environment,
##' and we can manipulate objects in this environment.
##'
##' The list of attributes in the brush (they can be accessed by the
##' \code{$} method):
##'
##' \describe{\item{style}{a list containing \code{color},
##' \code{linewidth} and \code{linetype} defining the style of the
##' brush (rectangle) -- not to be confused with the color of the
##' brushed elements} \item{color, size}{the color and size of the
##' brushed elements} \item{mode}{the brush mode: can be \code{'none'}
##' (default), \code{'and'}, \code{'or'}, \code{'xor'}, \code{'not'}
##' or \code{'complement'}; see \code{\link{mode_selection}} for
##' details} \item{identify}{logical: \code{TRUE} (turn on the
##' identify mode) or \code{FALSE} (the brush mode)}
##' \item{label.gen}{a function to be used to generate the labels
##' (based on the identified data) to show in the identify mode; the
##' default function just prints the identified data as a character
##' string} \item{label.color}{the color for the label in the identify
##' mode} \item{history.size}{the size of brush history, i.e. how many
##' brushing operations to be recorded; default to be 30}
##' \item{history.list}{the list of indices of the brushed elements;
##' we can go back and forth in the brush history according to this
##' list} \item{history.index}{the current index of the brush history}
##' \item{persistent}{persistent (\code{TRUE}) or transient
##' (\code{FALSE}) brushing; in the persistent brushing mode, the
##' attributes of the brushed elements will be changed permanently}
##' \item{persistent.color}{a color vector to store the colors of
##' persistently brushed elements} \item{persistent.list}{the
##' persistent brushing history (a list of indices of the brushed
##' elements)} \item{cursor}{the cursor type (an integer; see
##' \code{\link{set_cursor}})}}
##' @param data the mutaframe created by \code{\link{qdata}},
##' with an attribute \code{Brush}
##' @param attr the name of the brush attribute (a character scalar),
##' e.g. \code{style} (the color, linewidth and linetype of the
##' brush), \code{color} (the color of the objects selected by the
##' brush), \code{size} (the size of the selected objects, e.g. line
##' width or size of points); if \code{attr} is missing, the whole
##' brush object (as a reference object; see
##' \code{\link[methods]{setRefClass}}) will be returned
##' @return The function \code{\link{brush}} returns the brush object
##' or the attribute of the brush; note the brush object can be
##' further manipulated with other methods -- see examples below.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{qdata}}
##' @export
##' @examples library(cranvas)
##' qiris = qdata(head(iris))  # create a mutaframe
##' brush(qiris)  # the brush object (a reference object)
##' brush(qiris, 'color')
##' brush(qiris, 'color') = 'green'  # set brush color to green
##'
##' ## attach events on the brush
##' b = brush(qiris)
##'
##' #  idx is the index of the event; it can be used to stop the listening
##' idx = b$colorChanged$connect(function() {
##' message('the color of brushed elements was changed to ', b$color)})
##' b$color = 'brown'
##' b$color = 'gold'
##'
##' b$colorChanged$disconnect(idx)  # disconnect the event
##'
##' b$style$color = 'red'  # change the color of the brush itself to red
##' b$style$linewidth = 3  # the border width to 3
##'
##' b$mode = 'or'  # brush mode to OR
##'
##' b$history.size = 50  # increase history size to 50
##'
##' b$cursor = 3L  # cursor type to WaitCursor
##'
##' b$identify = TRUE  # turn on identify mode
##' b$identify = FALSE  # turn off; i.e. in brushing mode now
##'
##' b$persistent = TRUE  # turn on persistent brushing
##'
##' ## redefine label generating function: show row names in the identify mode
##' b$label.gen = function(x) {paste(rownames(x), collapse = ', ')}
##'
brush = function(data, attr) {
    if (!is.mutaframe(data) || !('Brush' %in% names(attributes(data))))
        stop("data must be a mutaframe and have an attribute 'Brush' (typically created by qdata())")
    .brush.attr = base::attr(data, 'Brush')
    if (missing(attr)) {
        .brush.attr
    } else {
        .brush.attr[[attr[1]]]
    }
}

##' @rdname brush
##' @usage brush(data, attr) <- value
##' @param value the value of the brush attribute
##' @export "brush<-"
`brush<-` = function (data, attr, value) {
    b = brush(data)
    eval(parse(text = paste('b$', attr[1], ' = value', sep = '')))
    data
}

##' Create the brush history
##'
##' Given the indices of the brushed elements, this function stores
##' these indices in the \code{\link{brush}} object and changes the
##' colors of graphical elements permanently (via changing the
##' \code{.color} column in the data) if in the persistent brushing
##' mode.
##'
##' For the transient brushing: the given indices are stored in the
##' \code{history.list} component of the brush object. The length of
##' the list of indices is restricted by the \code{history.size}
##' component of the brush, i.e., old histories may be removed due to
##' this size restriction.
##'
##' For the persistent brushing: the given indices of brushed elements
##' are stored in the \code{persistent.list} component, and the
##' current brushing color is also saved to the
##' \code{persistent.color} component. The colors of brushed elements
##' will be changed permanently. Finally, the length of the list of
##' indices is also restricted by the \code{history.size} component of
##' the brush.
##'
##' We can use these stored information to redraw the brushed elements
##' later. See \code{\link{brush}} for detailed explanation of these
##' components.
##' @param data the mutaframe created by \code{\link{qdata}}
##' @param index the indices of rows to be stored in history; an
##' integer vector or a logical vector (will be coerced to integers by
##' \code{\link[base]{which}}); by default it is
##' \code{selected(data)}, i.e., the logical vector indicating which
##' rows are brushed
##' @return the \code{data} argument will be returned and other
##' changes occur as side effects
##' @author Yihui Xie <\url{http://yihui.name}>
##' @note The changes occur only if the \code{index} argument is not
##' empty, or when the \code{data} argument is in the persistent
##' brushing mode, i.e., when \code{brush(data, 'persistent')} is
##' \code{TRUE}. In this case, the returned \code{data} will be
##' different with the one passed in, because the brush object
##' attached on it has been changed.
##' @export
##' @seealso \code{\link{brush}}, \code{\link{qdata}}, \code{\link{selected}}
##' @examples library(cranvas)
##' data(nrcstat)
##' qnrc = qdata(nrcstat)
##' selected(qnrc)  # all FALSE by default
##' selected(qnrc)[1:5] = TRUE  # brush first 5 rows
##'
##' b = brush(qnrc)  # the brush object
##' b$history.list  # this list should be empty by default
##'
##' save_brush_history(qnrc)  # store currently brushed row indices in history
##' save_brush_history(qnrc, c(6, 7, 10))  # another history
##'
##' b$history.list  # what happened to the brush object?
##'
##' b$persistent = TRUE  # turn on persistent brushing
##' b$persistent.list  # this list should be empty by default too
##' save_brush_history(qnrc, c(3, 4, 6, 9))  # permanently brush other 4 rows
##'
##' b$persistent.list  # what happened to the brush object?
##' b$persistent.color
##' b$color
##' b$history.list
##'
save_brush_history = function(data, index = selected(data)) {
    b = brush(data)
    if (is.logical(index)) index = which(index)
    csize = length(b$history.list) + 1
    if (length(index) > 0)
        b$history.list[[csize]] = index
    ## remove the first few columns due to the history size limit
    if (csize > (hsize <- b$history.size)) {
        b$history.list[seq_len(csize - hsize)] = NULL
    }
    b$history.index = length(b$history.list)
    ## persistent brushing
    if (b$persistent) {
        csize = length(b$persistent.list) + 1
        if (length(index) > 0) {
            b$persistent.list[[csize]] = index
            b$persistent.color[csize] = b$color
            data$.color[index] = b$color
        }
        if (csize > hsize) {
            b$persistent.list[seq_len(csize - hsize)] = NULL
            b$persistent.color = b$persistent.color[-seq_len(csize - hsize)]
        }
    }
    invisible(data)
}

##' Update the brush size in the mouse move event
##'
##' The brush size is changed by the differences in two successive
##' mouse positions.
##'
##' The current mouse position is obtained from \code{event$pos()}. If
##' the brush is in the move mode (\code{meta$brush.move == TRUE};
##' often set in a mouse click event), the brush size is updated by
##' the differences between \code{meta$start} and \code{meta$pos}; the
##' former is the starting position of the mouse, and the latter is
##' the current position.
##' @param meta the meta data containing the brush information
##' (\code{meta$pos}, \code{meta$brush.size}, \code{meta$brush.move}
##' and \code{meta$start})
##' @param event the event in the callback (if missing, it will search
##' in the parent environment \code{sys.frame(1)} which is often the
##' callback function)
##' @return a matrix of the coordinates of the brush rectangle, which
##' can be passed to \code{\link[qtbase]{qrect}} and used to query the
##' brushed elements by \code{layer$locate()}; as a side effect, the
##' brush size is updated, unless it is only a single click or the
##' brush is not in the move mode
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples ## see source code of qparallel()
update_brush_size = function(meta, event) {
    if (missing(event)) event = get('event', sys.frame(1))  # get event from the callback
    meta$pos = as.numeric(event$pos())
    if (length(meta$start) == 0) meta$start = meta$pos
    ## simple click: don't change meta$brush.size
    if (!all(meta$pos == meta$start)) {
        if (length(meta$brush.move) && !meta$brush.move) {
            meta$brush.size = meta$brush.size + meta$pos - meta$start
            meta$start = meta$pos
        }
    }
    matrix(c(meta$pos - meta$brush.size, meta$pos), 2, byrow = TRUE)
}

##' Manually brush the plot via command line
##'
##' We can brush a plot via command line rather than using the mouse.
##'
##' @param obj the plot object with an attribute \code{meta},
##' e.g. returned by \code{\link{qbar}}
##' @param pos the mouse position(s); can be a numeric vector of
##' length 2 or a matrix of 2 columns with each row representing a
##' mouse position
##' @param pause the time to pause between two successive mouse
##' positions
##' @return \code{NULL}
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example inst/examples/manual_brush.R
manual_brush = function(obj, pos, pause = 0) {
    meta = attr(obj, 'meta')
    if (is.null(meta)) stop("obj must have an attribute 'meta'")
    pos = matrix(pos, ncol = 2)
    for (i in 1:nrow(pos)) {
        meta$manual.brush(pos[i, ])
        Sys.sleep(pause)
    }
}

##' Draw the brush rectangle
##'
##' Draw a rectangle with a spot according to the information in the
##' meta data.
##'
##' @param layer,painter the painter of the layer on which to draw the
##' brush
##' @param data the data created by \code{\link{qdata}}; a brush
##' object is in \code{brush(data)}; the color and line width of the
##' brush are stored in this object
##' @param meta the meta data (has a least these two components:
##' \code{meta$pos} and \code{meta$brush.size})
##' @return \code{NULL}
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples ## see the source code of, e.g., qbar() for its usage
draw_brush = function(layer, painter, data, meta) {
    if (length(meta$pos) == 0) return()
    b = brush(data)
    qlineWidth(painter) = b$style$linewidth
    qdrawRect(painter, meta$pos[1] - meta$brush.size[1],
              meta$pos[2] - meta$brush.size[2], meta$pos[1], meta$pos[2],
              stroke = b$style$color, fill = NA)
    qdrawCircle(painter, meta$pos[1], meta$pos[2], r = 1.5 * b$style$linewidth,
                stroke = b$style$color, fill = b$style$color)
}
