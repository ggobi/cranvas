##' Parallel coordinates plot.
##' Create a parallel coordinates plot from a data frame or matrix, with each
##' line representing a row.
##'
##' We can turn on/off identify by setting \code{brush(data,
##' 'identify')} to be \code{TRUE}/\code{FALSE}. In the identify mode,
##' the rows of data in a small neighborhood of the mouse will be
##' shown on the screen. In the brush mode, we can use the left button
##' of the mouse to drag a brush over the plot, and the brushed
##' elements will be highlighted; the right button is used to resize
##' the brush. A simple click will not activate the brush -- the brush
##' works only when the mouse is moved to a difference location with
##' one button being pressed.
##'
##' Several key stroke interactions are available as well: in the
##' brush mode, we can hold the key \code{A} for \code{AND}
##' operations, i.e. only the elements which were brushed in the last
##' time \emph{and} are also brushed this time will be finally
##' brushed. Similarly, \code{O} is for \code{OR}; \code{X} for
##' \code{XOR}; \code{N} for \code{NOT} and \code{C} for
##' \code{COMPLEMENT}. We can press \code{R} to toggle the min/max
##' labels. Keys \code{+} and \code{-} can adjust the opacity of the
##' plot linearly. \code{Delete} can make the brushed elements
##' invisible while \code{F5} will make all the elements visible. The
##' arrow keys are used to adjust the order of the variables and flip
##' the values of variables (like a mirror reflection). \code{PageUp}
##' and \code{PageDown} can be used to go back and forth in the brush
##' history.
##' @param vars variables to show; can be a character vector (column
##' names), an integer vector (column indices) or a formula like '~ x1
##' + x2'; if missing or it is a formula that contains a dot
##' (e.g. \code{ ~ .}), all variables in the data except those whose
##' names start with a dot will be used
##' @param data a mutaframe which is typically built upon a data frame
##' along with several row attributes
##' @param scale standardizing method - 'range' --> [0, 1], 'I' --> do
##' nothing, 'var' --> mean 0 var 1, 'custom_function_name' --> use
##' your own function (see examples.R)
##' @param names the variable labels to use in the plot (by default,
##' they are the variable names with non-alphanumeric characters
##' replaced by line breaks \code{'\n'})
##' @param na.action the function to deal with missing values
##' @param center the function to calculate where to center all the
##' variables (e.g. center at the medians), or a numeric value, or
##' \code{NULL} (do not center)
##' @param order methods to reorder the variables; see \code{\link{reorder_var}}
##' @param horizontal logical: arrange variables in horizontal or
##' vertical direction
##' @param glyph draw complete segments for all observations or other
##' types of glyphs to represent observations (the latter can be more
##' efficient in case of large data)
##' @param boxplot logical: overlay boxplots on top of the par-coords
##' plot or not
##' @param boxwex width of boxplots
##' @param jitter NULL (no jittering) or a character vector to jitter
##' variables (usually those categorical vars)
##' @param amount jitter amount
##' @param main the title
##' @param alpha the opacity value
##' @param draw.range whether to draw the range values (min and max
##' for each variable)
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example cranvas/inst/examples/qparallel-ex.R
qparallel = function(vars, data, scale = "range", names = break_str(vars),
    na.action = na.impute,
    center = NULL, order = c('none', 'MDS', 'ANOVA', 'randomForest'), horizontal = TRUE,
    glyph = c('auto', 'line', 'tick', 'circle', 'square', 'triangle'),
    boxplot = FALSE, boxwex, jitter = NULL, amount = NULL,
    main, alpha = 1, draw.range = TRUE) {

    b = brush(data)    # the brush attached to the data

    if (missing(main)) {
        main = paste("Parallel Coordinates Plot of", deparse(substitute(data))) # title
    }

    if (missing(vars)) vars = grep('^[^.]', colnames(data), value = TRUE)
    if (class(vars) == "formula") {
        vars = all.vars(vars)
        if ('.' %in% vars) vars = grep('^[^.]', colnames(data), value = TRUE)
    }
    if (is.numeric(vars)) vars = colnames(data)[vars]
    if (length(vars) <= 1L)
        stop("parallel coordinate plots need at least 2 variables!")

    ## meta data used to store useful information
    meta = Parallel.meta$new(brush.move = TRUE, alpha = alpha,
                    main = main, vars = vars, glyph = match.arg(glyph),
                    order = match.arg(order), draw.range = draw.range,
                    jitter = jitter, amount = amount, names = names)

    data_preprocess = function() {
        tmp = as.data.frame(data[, meta$vars], stringsAsFactors = TRUE)
        tmp = .rm.cons.col(tmp)  # remove constant columns
        if (length(meta$vars <- names(tmp)) <= 1)
            stop('there are less than 2 variables in the data')
        ## which columns are numeric? we don't want boxplots for non-numeric vars
        meta$numeric.col = sapply(tmp, is.numeric)

        meta$plot.data = sapply(tmp, as.numeric)
        meta$p = ncol(meta$plot.data)
        meta$n = nrow(meta$plot.data)

        meta$plot.data = na.action(meta$plot.data) # handle missing values

        if (length(meta$jitter)) {
            meta$plot.data[, meta$jitter] =
                apply(meta$plot.data[, meta$jitter, drop = FALSE],
                      2, base::jitter, amount = meta$amount)  # jittering
        }

        scale = switch(scale, range = function(x) {
            xna = x[!is.na(x)]
            (x - min(xna))/(max(xna) - min(xna))
        }, var = base::scale, I = identity, get(scale))
        meta$plot.data = apply(meta$plot.data, 2, scale)  # standardizing

        if (!is.null(center)){
            meta$plot.data = apply(meta$plot.data, 2, function(x) {
                x - ifelse(is.function(center), center(x), as.numeric(center))  # centering
            })
        }
    }

    ## final calculations for graphical primitives, e.g. segments, axis labels
    data_primitives = function() {
        ## switch x and y according to the direction
        if (horizontal) {
            meta$x = col(meta$plot.data)
            meta$y = meta$plot.data
            meta$xat = 1:meta$p
            meta$xlabels = meta$names
            meta$yat = axis_loc(meta$y)
            meta$ylabels = format(meta$yat)
        } else {
            meta$y = col(meta$plot.data)
            meta$x = meta$plot.data
            meta$yat = 1:meta$p
            meta$ylabels = meta$names
            meta$xat = axis_loc(meta$x)
            meta$xlabels = format(meta$xat)
        }
        meta$xr = diff(range(meta$x))
        meta$yr = diff(range(meta$y))
        ## extend margins
        meta$limits = extend_ranges(matrix(c(range(meta$x), range(meta$y)), 2))

        ## 'auto' means 'line's when n*p<=5000*10, and 'tick's otherwise
        if (meta$glyph == 'auto')
            meta$glyph = ifelse(meta$n * meta$p <= 50000, 'line', 'tick')
        if (meta$glyph == 'line') {
            ## creating starting and ending vectors, because indexing in real-time is slow
            meta$segx0 = as.vector(t.default(meta$x[, -meta$p]))
            meta$segx1 = as.vector(t.default(meta$x[, -1]))
            meta$segy0 = as.vector(t.default(meta$y[, -meta$p]))
            meta$segy1 = as.vector(t.default(meta$y[, -1]))
        } else {
            meta$x0 = as.vector(t.default(meta$x))
            meta$y0 = as.vector(t.default(meta$y))
        }
    }

    ## given orders, rearrange the data
    ## need to update: numcol, plot_data, vars, boxplot data, primitives data
    data_reorder = function(vars) {
        tmp = meta$numeric.col
        names(tmp) = colnames(meta$plot.data)
        meta$numeric.col = tmp[vars]
        tmp = meta$names
        names(tmp) = colnames(meta$plot.data)
        meta$names = tmp[vars]
        meta$plot.data = meta$plot.data[, vars]
        meta$vars = colnames(meta$plot.data)
        data_primitives()
    }

    ## do the transformation now
    data_preprocess()

    ## order by MDS or ANOVA
    data_reorder(reorder_var(data = meta$plot.data, type = meta$order, vars = meta$vars,
                             numcol = meta$numeric.col, x = data$.color))

    ## brush range: horizontal and vertical
    meta$brush.size = c(meta$xr, -meta$yr)/15

    ## automatic box width
    if (missing(boxwex))
        boxwex = max(1/meta$p, 0.2)

    ## convention of notation:
    ## *_draw means a drawing function for a layer; *_event is an even callback; *_layer is a layer object

    draw.glyph = switch(meta$glyph, tick = qglyphSegment(d = ifelse(horizontal, 0, pi/2)),
                        circle = qglyphCircle(), square = qglyphSquare(),
                        triangle = qglyphTriangle())

    ## par-coords segments
    main_draw = function(layer, painter) {
        cranvas_debug()
        .color = data$.color
        .color[!visible(data)] = NA
        layer$setOpacity(meta$alpha)
        if (meta$glyph == 'line') {
            segcol = rep(.color, each = meta$p - 1)
            qdrawSegment(painter, meta$segx0, meta$segy0, meta$segx1, meta$segy1,
                         stroke = segcol)
        } else {
            main.col = rep(.color, each = meta$p)
            qdrawGlyph(painter, draw.glyph, meta$x0, meta$y0, stroke = main.col)
        }
        cranvas_debug()
    }

    ## annotate maximum and minimum values for each axis
    range_draw = function(layer, painter) {
        if (!meta$draw.range) return()
        if (any(meta$numeric.col)) {
            dat = as.data.frame(data)[, meta$vars][, meta$numeric.col]
            range.d = round(as.matrix(apply(dat, 2, range, na.rm=TRUE)), 2)
            numcol = which(meta$numeric.col)
            qstrokeColor(painter) = data$.color[1]
            if (horizontal) {
                qdrawText(painter, range.d[1, ], numcol, meta$limits[3], valign = 'bottom')
                qdrawText(painter, range.d[2, ], numcol, meta$limits[4], valign = 'top')
            } else {
                qdrawText(painter, range.d[1, ], meta$limits[1], numcol, halign = 'left')
                qdrawText(painter, range.d[2, ], meta$limits[2], numcol, halign = 'right')
            }
        }
    }

    ## record the coordinates of the mouse on click
    brush_mouse_press = function(layer, event) {
        meta$start = as.numeric(event$pos())
        ## on right click, we can resize the brush; left click: only move the brush
        if (event$button() == Qt$Qt$RightButton) {
            b$cursor = 2L
            meta$brush.move = FALSE
        } else if (event$button() == Qt$Qt$LeftButton) {
            b$cursor = 0L
            meta$brush.move = TRUE
        }
    }

    ## monitor keypress event
    brush_key_press = function(layer, event) {
        key = event$key()
        ## Key X: XOR; O: OR; A: AND; N: NOT
        i = match_key(c('A', 'O', 'X', 'N', 'C'), logical = FALSE)
        if (length(i))
            b$mode = c('and', 'or', 'xor', 'not', 'complement')[i]

        ## change opacity of layer: + or -
        i = match_key(c('Plus', 'Minus'), logical = FALSE)
        if (length(i)) {
            meta$alpha = max(0.01, min(1, c(1.1, 0.9)[i] * meta$alpha))
            layer.main$setOpacity(meta$alpha)
            qupdate(layer.main)
        }

        ## whether to draw min/max labels
        if (match_key('R')) {
            meta$draw.range = !meta$draw.range
            qupdate(layer.range)
            return()
        }

        if (match_key('Delete'))
            visible(data) = !selected(data) & visible(data)  # make brushed obs invisible
        if (match_key('F5'))
            visible(data) = TRUE  # make all of them visible

        i = match_key(c('Left', 'Right', 'Down', 'Up'), logical = FALSE)
        if (length(i) && !any(is.na(meta$pos))) {
            if (horizontal) {
                j = 1
                movedir = switch(i, -1, 1, NULL, NULL)
                flipdir = switch(i, NULL, NULL, -1, 1)
            } else {
                j = 2
                movedir = switch(i, NULL, NULL, -1, 1)
                flipdir = switch(i, -1, 1, NULL, NULL)
            }
            xs = which((1:meta$p) > meta$pos[j] - meta$brush.size[j] & (1:meta$p) < meta$pos[j] + meta$brush.size[j])
            if ((nxs <- length(xs))) {
                if (!is.null(movedir)) {
                    vars0 = meta$vars
                    if (xs[1] > 1 & movedir == -1){
                        vars0[c(xs[1] - 1, xs)] = vars0[c(xs, xs[1] - 1)]
                        meta$pos[j] = meta$pos[j] - 1
                    }
                    if (xs[nxs] < meta$p & movedir == 1){
                        vars0[c(xs, xs[nxs] + 1)] = vars0[c(xs[nxs] + 1, xs)]
                        meta$pos[j] = meta$pos[j] + 1
                    }
                    if (any(vars0 != meta$vars)) {
                        data_reorder(vars0)
                        data_primitives()
                        qupdate(layer.xaxis)
                        qupdate(layer.yaxis)
                        layer.main$invalidateIndex()
                        qupdate(layer.main)
                        qupdate(layer.brush)
                        if (boxplot) {
                            qupdate(layer.boxplot)
                        }
                    }
                }
                if (!is.null(flipdir)) {
                    meta$plot.data[, xs] = apply(meta$plot.data[, xs, drop = FALSE], 2,
                                              function(xx) {
                                                  meta$limits[c(3,1)[j]] + meta$limits[c(4,2)[j]] - xx
                                              })
                    data_primitives()
                    qupdate(layer.xaxis)
                    qupdate(layer.yaxis)
                    layer.main$invalidateIndex()
                    qupdate(layer.main)
                    qupdate(layer.brush)
                    if (boxplot) {
                        qupdate(layer.boxplot)
                    }
                }
            }
        }
        ## data range labels
    }
    brush_key_release = function(layer, event) {
        b$mode = 'none'  # set brush mode to 'none' when release the key
        direction = match_key(c('PageUp', 'PageDown'), logical = FALSE)
        if (length(direction)) {
            idx = b$history.index + c(-1, 1)[direction]
            idx = max(1, min(length(b$history.list), idx))
            b$history.index = idx
            selected(data) = b$history.list[[idx]]
        }
    }

    ## identify segments being brushed when the mouse is moving
    brush_mouse_move = function(layer, event) {
        cranvas_debug()
        if (b$identify) return()
        rect = qrect(update_brush_size(meta))
        hits = layer$locate(rect) + 1
        ## ticks and lines are of different numbers!
        hits = ceiling(hits/ifelse(meta$glyph == 'line', meta$p - 1, meta$p))
        selected(data) = mode_selection(selected(data), hits, mode = b$mode)
        self_link(data)
        cranvas_debug()
    }
    brush_mouse_release = function(layer, event) {
        brush_mouse_move(layer, event)
        b$cursor = 0L  # restore to Arrow cursor
        save_brush_history(data)  # store brushing history
    }

    ## convert a matrix to coordinates of segments
    mat2seg = function(x, idx = 1:nrow(x)) {
        x = x[idx, , drop = FALSE]
        as.vector(t.default(cbind(x, NA)))
    }

    ## draw the segments under the brush with another appearance
    brush_draw = function(layer, painter) {
        cranvas_debug()
        if (b$identify) return()
        .visible = which(visible(data))
        if (b$persistent && length(b$persistent.list)) {
            qlineWidth(painter) = b$size
            for (i in seq_along(b$persistent.list)) {
                idx = intersect(b$persistent.list[[i]], .visible)
                if (!length(idx)) next
                qstrokeColor(painter) = b$persistent.color[i]
                tmpx = mat2seg(meta$x, idx)
                tmpy = mat2seg(meta$y, idx)
                nn = length(tmpx)
                qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
            }
        }
        .brushed = intersect(which(selected(data)), .visible)
        if (length(.brushed)) {
            qlineWidth(painter) = b$size
            qstrokeColor(painter) = b$color
            tmpx = mat2seg(meta$x, .brushed)
            tmpy = mat2seg(meta$y, .brushed)
            nn = length(tmpx)
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
        }
        draw_brush(painter, b, meta)
        cranvas_debug()
    }

    identify_hover = function(layer, event) {
        if (!b$identify) return()
        b$cursor = 2L  # Cross
        meta$pos = as.numeric(event$pos())
        rect = qrect(matrix(c(meta$pos - c(meta$xr, meta$yr)/100, meta$pos + c(meta$xr, meta$yr)/100), 2, byrow = TRUE))
        hits = layer$locate(rect) + 1
        meta$identified = ceiling(hits/ifelse(meta$glyph == 'line', meta$p - 1, meta$p))
        qupdate(layer.identify)
    }

    identify_draw = function(layer, painter) {
        if (b$identify && length(meta$identified)) {
            qlineWidth(painter) = b$size
            qstrokeColor(painter) = b$color
            tmpx = mat2seg(meta$x, meta$identified)
            tmpy = mat2seg(meta$y, meta$identified)
            nn = length(tmpx)
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
            qfont(painter) = Qt$QFont('Monospace')
            .labels = b$label.gen(data[meta$identified, meta$vars])
            bgwidth = qstrWidth(painter, .labels)
            bgheight = qstrHeight(painter, .labels)
            ## adjust drawing directions when close to the boundary
            hflag = meta$limits[2] - meta$pos[1] > bgwidth
            vflag = meta$pos[2] - meta$limits[3] > bgheight
            qdrawRect(painter, meta$pos[1], meta$pos[2],
                      meta$pos[1] + ifelse(hflag, 1, -1) * bgwidth,
                      meta$pos[2] + ifelse(vflag, -1, 1) * bgheight,
                      stroke = rgb(1, 1, 1, 0.8), fill = rgb(1, 1, 1, 0.8))
            qstrokeColor(painter) = b$label.color
            qdrawText(painter, .labels, meta$pos[1], meta$pos[2],
                      halign = ifelse(hflag, "left", "right"),
                      valign = ifelse(vflag, "top", "bottom"))
        }
    }

    scene = qscene()
    layer.root = qlayer(scene)

    layer.main = qlayer(paintFun = main_draw,
        mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_release,
        mouseMove = brush_mouse_move, keyPressFun = brush_key_press,
        keyReleaseFun = brush_key_release, hoverMoveFun = identify_hover,
        focusInFun = function(layer, painter) {
            focused(data) = TRUE
        }, focusOutFun = function(layer, painter) {
            focused(data) = FALSE
        },
        limits = qrect(meta$limits))

    layer.range = qlayer(paintFun = range_draw, limits = qrect(meta$limits))
    layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
    layer.identify = qlayer(paintFun = identify_draw, limits = qrect(meta$limits))
    layer.title = qmtext(data = meta, side = 3, text = main, sister = layer.main)
    layer.xaxis = qaxis(data = meta, side = 1, sister = layer.main)
    layer.yaxis = qaxis(data = meta, side = 2, sister = layer.main)
    layer.grid = qgrid(data = meta, sister = layer.main,
                       minor = ifelse(horizontal, 'y', 'x'))
    layer.legend = qlayer()  # legend layer (currently only acts as place holder)

    layer.root[0, 1] = layer.title
    layer.root[2, 1] = layer.xaxis
    layer.root[1, 0] = layer.yaxis
    layer.root[1, 1] = layer.grid
    if (boxplot) {
        layer.boxplot = qbxp(data = meta, width = boxwex, horizontal = !horizontal,
                             sister = layer.main)
        layer.root[1, 1] = layer.boxplot
    }
    layer.root[1, 1] = layer.main
    layer.root[1, 1] = layer.range
    layer.root[1, 1] = layer.brush
    layer.root[1, 1] = layer.identify
    layer.root[1, 2] = layer.legend

    ## update the brush layer in case of any modifications to the mutaframe
    d.idx = add_listener(data, function(i, j) {
        switch(j, .brushed = qupdate(layer.brush),
               .color = qupdate(layer.main), {
                   data_preprocess()
                   data_primitives()
                   qupdate(layer.grid)
                   qupdate(layer.xaxis)
                   qupdate(layer.yaxis)
                   qupdate(layer.main)
                   if (boxplot) {
                       qupdate(layer.boxplot)
                   }
               })
    })

    layout = layer.root$gridLayout()
    layout$setRowPreferredHeight(0, 30)
    ## the y-axis layer needs 'dynamic' width determined by #{characters}
    ## here is a formula by my rule of thumb: 9 * nchar + 5
    layout$setColumnPreferredWidth(0, 9 * max(nchar(unlist(strsplit(meta$ylabels, '\n')))) + 5)
    layout$setRowPreferredHeight(2, 15 * max(sapply(gregexpr('\\n', meta$xlabels),
                              function(xx) ifelse(any(xx <0), 0, length(xx)) + 2)))
    layout$setColumnMaximumWidth(2, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setColumnStretchFactor(0, 0)
    layout$setRowStretchFactor(2, 0)

    view = qplotView(scene = scene)
    view$setWindowTitle(meta$main)
    attr(view, 'meta') = meta

    ## update the brush layer if brush attributes change
    b.idx = b$colorChanged$connect(function() {
        qupdate(layer.brush)
    })

    qconnect(layer.main, 'destroyed', function(x) {
        b$colorChanged$disconnect(b.idx)
        remove_listener(data, d.idx)
    })
    ## change the cursor
    b$cursorChanged$connect(function() {
        set_cursor(view, b$cursor)
    })
    ## more attributes to come
    meta$manual.brush = function(pos) {
        brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
    }

    view
}

Parallel.meta =
    setRefClass("Parallel_meta",
                fields = signalingFields(list(
                pos = 'numeric', start = 'numeric', brush.move = 'logical',
                alpha = 'numeric', main = 'character', vars = 'character',
                glyph = 'character', order = 'character', draw.range = 'logical',
                plot.data = 'matrix', numeric.col = 'logical', p = 'numeric', n = 'numeric',
                jitter = 'character', amount = 'numeric', x = 'matrix', y = 'matrix',
                xr = 'numeric', yr = 'numeric', names = 'character',
                xat = 'numeric', yat = 'numeric',
                xlabels = 'character', ylabels = 'character', limits = 'matrix',
                segx0 = 'numeric', segx1 = 'numeric', segy0 = 'numeric', segy1 = 'numeric',
                x0 = 'numeric', y0 = 'numeric', brush.size = 'numeric',
                identified = 'numeric', manual.brush = 'function')))
