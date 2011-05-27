##' Parallel coordinates plot.
##' Create a parallel coordinates plot from a data frame or matrix, with each
##' line representing a row.
##'
##' Press R to toggle the min/max labels.
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
    meta = mutalist(pos = c(NA, NA), start = c(NA, NA), brush.move = TRUE, alpha = alpha,
                    main = main, vars = vars, glyph = match.arg(glyph),
                    order = match.arg(order), draw.range = draw.range,
                    plot.data = NULL, numeric.col = NULL, p = NULL, n = NULL,
                    jitter = jitter, amount = amount,
                    x = NULL, y = NULL, xr = NULL, yr = NULL, names = names,
                    xat = NULL, yat = NULL, xlabels = NULL, ylabels = NULL,
                    limits = NULL,
                    segx0 = NULL, segx1 = NULL, segy0 = NULL, segy1 = NULL,
                    x0 = NULL, y0 = NULL, brush.range = c(NA, NA)
    )

    ## a long way of transformation
    ## creat some 'global' variables first
    bxpstats = NULL

    data_preprocess = function() {
        meta$plot.data = as.data.frame(data[, meta$vars], stringsAsFactors = TRUE)
        meta$plot.data = .rm.cons.col(meta$plot.data)  # remove constant columns
        if (length(meta$vars <- names(meta$plot.data)) <= 1)
            stop('there are less than 2 variables in the data')
        ## which columns are numeric? we don't want boxplots for non-numeric vars
        meta$numeric.col = sapply(meta$plot.data, is.numeric)

        meta$plot.data = sapply(meta$plot.data, as.numeric)
        meta$p = ncol(meta$plot.data)
        meta$n = nrow(meta$plot.data)

        meta$plot.data = na.action(meta$plot.data) # handle missing values

        if (!is.null(meta$jitter)) {
            if (class(meta$jitter) == "formula")
                meta$jitter = attr(terms(meta$jitter, data = meta$plot.data), "term.labels")
            if (is.numeric(meta$jitter)) meta$jitter = names(data)[meta$jitter]
            if (is.character(meta$jitter)) {
                meta$plot.data[, meta$jitter] =
                    apply(meta$plot.data[, meta$jitter, drop = FALSE],
                          2, base::jitter, amount = meta$amount)  # jittering
            }
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
            meta$yat = .axis.loc(meta$y)
            meta$ylabels = format(meta$yat)
        } else {
            meta$y = col(meta$plot.data)
            meta$x = meta$plot.data
            meta$yat = 1:meta$p
            meta$ylabels = meta$names
            meta$xat = .axis.loc(meta$x)
            meta$xlabels = format(meta$xat)
        }
        meta$xr = diff(range(meta$x))
        meta$yr = diff(range(meta$y))
        ## extend margins
        meta$limits = .extend.ranges(matrix(c(range(meta$x), range(meta$y)), 2))

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

    data_boxplot = function() {
        ## for boxplots
        if (boxplot)
            bxpstats <<- apply(meta$plot.data, 2,
                               function(x) boxplot.stats(x, do.conf = FALSE)$stats)
    }

    ## given orders, rearrange the data
    ## need to update: numcol, plot_data, vars, boxplot data, primitives data
    data_reorder = function(vars) {
        numcol = meta$numeric.col
        names(numcol) = colnames(meta$plot.data)
        meta$numeric.col = numcol[vars]
        meta$plot.data = meta$plot.data[, vars]
        meta$vars = colnames(meta$plot.data)
        data_boxplot()
        data_primitives()
    }

    ## do the transformation now
    data_preprocess()

    ## order by MDS or ANOVA
    data_reorder(reorder_var(data = meta$plot.data, type = meta$order, vars = meta$vars,
                             numcol = meta$numeric.col, x = data$.color))

    ## brush range: horizontal and vertical
    meta$brush.range = c(meta$xr, meta$yr)/30

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
        if (any(meta$numeric.col)) {
            dat = as.data.frame(data)[, meta$vars][, meta$numeric.col]
            if (!meta$draw.range) return()
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

    ## (optional) boxplots
    boxplot_draw = function(layer, painter) {
        cranvas_debug()
        qstrokeColor(painter) = "black"
        for (i in (1:meta$p)[meta$numeric.col]) {
            x0 = c(rep(i - boxwex/2, 3), rep(i, 2))
            y0 = bxpstats[c(1, 3, 5, 1, 4), i]
            x1 = c(rep(i + boxwex/2, 3), rep(i, 2))
            y1 = bxpstats[c(1, 3, 5, 2, 5), i]
            xleft = i - boxwex/2
            ybottom = bxpstats[2, i]
            xright = i + boxwex/2
            ytop = bxpstats[4, i]
            ## exchange x and y if vertical
            if (!horizontal) {
                tmp = x0; x0 = y0; y0 = tmp
                tmp = x1; x1 = y1; y1 = tmp
                tmp = xleft; xleft = ybottom; ybottom = tmp
                tmp = xright; xright = ytop; ytop = tmp
            }
            qdrawSegment(painter, x0, y0, x1, y1)
            qdrawRect(painter, xleft, ybottom, xright, ytop, fill = 'darkgray')
            qlineWidth(painter) = 3
            qdrawSegment(painter, x0[2], y0[2], x1[2], y1[2])
            qlineWidth(painter) = 1
        }
        cranvas_debug()
    }

    ## record the coordinates of the mouse on click
    brush_mouse_press = function(layer, event) {
        meta$start = as.numeric(event$pos())
        ## on right click, we can resize the brush; left click: only move the brush
        if (event$button() == Qt$Qt$RightButton) {
            meta$brush.move = FALSE
        } else if (event$button() == Qt$Qt$LeftButton) {
            meta$brush.move = TRUE
        }
    }

    ## monitor keypress event
    brush_key_press = function(layer, event) {
        key = event$key()
        ## Key X: XOR; O: OR; A: AND; N: NOT
        i = which(key == c(Qt$Qt$Key_A, Qt$Qt$Key_O, Qt$Qt$Key_X, Qt$Qt$Key_N, Qt$Qt$Key_C))
        if (length(i))
            b$mode = c('and', 'or', 'xor', 'not', 'complement')[i]

        ## change opacity of layer: + or -
        i = which(key == c(Qt$Qt$Key_Plus, Qt$Qt$Key_Minus))
        if (length(i)) {
            meta$alpha = max(0, min(1, c(0.01, -0.01)[i] + meta$alpha))
            layer.main$setOpacity(meta$alpha)
            qupdate(layer.main)
        }

        ## whether to draw min/max labels
        if (key == Qt$Qt$Key_R) {
            meta$draw.range = !meta$draw.range
            qupdate(layer.range)
            return()
        }

        ## make the brushed observations invisible when hitting Delete
        if (key == Qt$Qt$Key_Delete)
            visible(data) = !selected(data)
        i = which(key == c(Qt$Qt$Key_Left, Qt$Qt$Key_Right, Qt$Qt$Key_Down, Qt$Qt$Key_Up))
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
            xs = which((1:meta$p) > meta$pos[j] - meta$brush.range[j] & (1:meta$p) < meta$pos[j] + meta$brush.range[j])
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
                        qupdate(layer.xaxis)
                        qupdate(layer.yaxis)
                        layer.main$invalidateIndex()
                        qupdate(layer.main)
                        qupdate(layer.brush)
                        if (boxplot) {
                            data_boxplot()
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
                        data_boxplot()
                        qupdate(layer.boxplot)
                    }
                }
            }
        }
        ## data range labels
    }
    brush_key_release = function(layer, event) {
        b$mode = 'none'  # set brush mode to 'none' when release the key
        direction = which(event$key() == c(Qt$Qt$Key_PageUp, Qt$Qt$Key_PageDown))
        if (length(direction)) {
            idx = b$history.index + c(-1, 1)[direction]
            idx = max(1, min(length(b$history.list), idx))
            b$history.index = idx
            .brushed = logical(meta$n)
            .brushed[b$history.list[[idx]]] = TRUE
            selected(data) = .brushed
        }
    }

    ## identify segments being brushed when the mouse is moving
    brush_mouse_move = function(layer, event) {
        cranvas_debug()
        if (b$identify) return()
        meta$pos = as.numeric(event$pos())
        ## simple click: don't change meta$brush.range
        if (!all(meta$pos == meta$start) && (!meta$brush.move)) {
            meta$brush.range = meta$pos - meta$start
        }
        .new.brushed = rep(FALSE, meta$n)
        rect = qrect(matrix(c(meta$pos - meta$brush.range, meta$pos + meta$brush.range), 2, byrow = TRUE))
        hits = layer$locate(rect) + 1
        ## ticks and lines are of different numbers!
        hits = ceiling(hits/ifelse(meta$glyph == 'line', meta$p - 1, meta$p))
        .new.brushed[hits] = TRUE
        selected(data) = mode_selection(selected(data), .new.brushed, mode = b$mode)
        ## on mouse release
        if (event$button() != Qt$Qt$NoButton) {
            csize = length(b$history.list) + 1
            .cur.sel = which(selected(data))
            if (length(.cur.sel) > 0)
                b$history.list[[csize]] = .cur.sel
            ## remove the first few columns due to the history size limit
            if (csize > (hsize <- b$history.size)) {
                b$history.list[1:(csize - hsize)] = NULL
            }
            b$history.index = length(b$history.list)
            ## persistent brushing: store brushed objects
            if (b$persistent) {
                csize = length(b$persistent.list) + 1
                if (length(.cur.sel) > 0) {
                    b$persistent.list[[csize]] = .cur.sel
                    b$persistent.color[csize] = b$color
                    b$.color[.cur.sel] = b$color
                }
                if (csize > hsize) {
                    b$persistent.list[1:(csize - hsize)] = NULL
                    b$persistent.color = b$persistent.color[-(1:(csize - hsize))]
                }
            }
        }
        cranvas_debug()
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
        if (!any(is.na(meta$pos))) {
            qlineWidth(painter) = b$style$size
            ##qdash(painter)=c(1,3,1,3)
            qdrawRect(painter, meta$pos[1] - meta$brush.range[1], meta$pos[2] - meta$brush.range[2],
                      meta$pos[1] + meta$brush.range[1], meta$pos[2] + meta$brush.range[2],
                      stroke = b$style$color)
        }
        if (b$persistent && length(b$persistent.list)) {
            qlineWidth(painter) = b$size
            for (i in seq_along(b$persistent.list)) {
                idx = b$persistent.list[[i]]
                qstrokeColor(painter) = b$persistent.color[i]
                tmpx = mat2seg(meta$x, idx)
                tmpy = mat2seg(meta$y, idx)
                nn = length(tmpx)
                qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
            }
        }
        .brushed = selected(data)
        if (sum(.brushed, na.rm = TRUE) >= 1) {
            qlineWidth(painter) = b$size
            qstrokeColor(painter) = b$color
            tmpx = mat2seg(meta$x, .brushed)
            tmpy = mat2seg(meta$y, .brushed)
            nn = length(tmpx)
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])

       }
        cranvas_debug()
    }

    .identified = NULL
    identify_hover = function(layer, event) {
        if (!b$identify) return()
        meta$pos = as.numeric(event$pos())
        rect = qrect(matrix(c(meta$pos - c(meta$xr, meta$yr)/100, meta$pos + c(meta$xr, meta$yr)/100), 2, byrow = TRUE))
        hits = layer$locate(rect) + 1
        .identified <<- ceiling(hits/ifelse(meta$glyph == 'line', meta$p - 1, meta$p))
        qupdate(layer.identify)
    }

    identify_draw = function(layer, painter) {
        if (b$identify && length(.identified)) {
            qlineWidth(painter) = b$size
            qstrokeColor(painter) = b$color
            tmpx = mat2seg(meta$x, .identified)
            tmpy = mat2seg(meta$y, .identified)
            nn = length(tmpx)
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
            qfont(painter) = Qt$QFont('Monospace')
            .labels = b$label.gen(data[.identified, meta$vars])
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

    ## title
    title_layer = qlayer(layer.root, function(layer, painter) {
        qdrawText(painter, meta$main, (meta$limits[1] + meta$limits[2])/2, 0, "center", "bottom")
    }, limits = qrect(c(meta$limits[1], meta$limits[2]), c(0, 1)), row = 0, col = 1)

    layer.main = qlayer(paintFun = main_draw,
        mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_move,
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
    layer.xaxis = qaxis(data = meta, side = 1, sister = layer.main)
    layer.yaxis = qaxis(data = meta, side = 2, sister = layer.main)
    layer.grid = qgrid(data = meta, sister = layer.main,
                       minor = ifelse(horizontal, 'y', 'x'))
    layer.legend = qlayer()  # legend layer (currently only acts as place holder)

    layer.root[2, 1] = layer.xaxis
    layer.root[1, 0] = layer.yaxis
    layer.root[1, 1] = layer.grid
    if (boxplot) {
        layer.boxplot = qlayer(paintFun = boxplot_draw, limits = qrect(meta$limits))
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
                       data_boxplot()
                       qupdate(layer.boxplot)
                   }
               })
    })

    ## update the brush layer if brush attributes change
    brush_update = function() {
        qupdate(layer.brush)
    }
    b.idx = b$colorChanged$connect(brush_update)
    qconnect(layer.main, 'destroyed', function(x) {
        b$colorChanged$disconnect(b.idx)
        remove_listener(data, d.idx)
    })
    ## more attributes to come

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
    view
}
