##' Parallel coordinates plot.
##' Create a parallel coordinates plot from a data frame or matrix, with each
##' line representing a row.
##'
##' Press R to toggle the min/max labels.
##' @param vars variables to show; can be a character vector (column
##' names), an integer vector (column indices) or a formula like '~ x1
##' + x2'
##' @param data a mutaframe which is typically built upon a data frame
##' along with several row attributes
##' @param scale standardizing method - 'range' --> [0, 1], 'I' --> do
##' nothing, 'var' --> mean 0 var 1, 'custom_function_name' --> use
##' your own function (see examples.R)
##' @param na.action the function to deal with missing values
##' @param center the function to calculate where to center all the
##' axes (e.g. center at the medians), or a numeric value, or
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
##' @param lab.split the pattern to ``break'' the axis labels by
##' \code{'\n'}; the default \code{'[^[:alnum:]]'} means any
##' characters which are not alphanumeric will be replaced by
##' \code{'\n'}, i.e. the labels will be broken into several lines;
##' this can be useful when the axis labels are too long (if we do not
##' break them, they will be squeezed together along the axes); set
##' \code{lab.split = NULL} to keep the labels untouched
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example cranvas/inst/examples/qparallel-ex.R
qparallel = function(vars, data, scale = "range", na.action = na.impute,
    center = NULL, order = c('none', 'MDS', 'ANOVA', 'randomForest'), horizontal = TRUE,
    glyph = c('auto', 'line', 'tick', 'circle', 'square', 'triangle'),
    boxplot = FALSE, boxwex, jitter = NULL, amount = NULL,
    main, lab.split = '[^[:alnum:]]', alpha=1, ...) {

    b = brush(data)    # the brush attached to the data

    ## mouse position
    .bpos = c(NA, NA)
    ## drag start
    .bstart = c(NA, NA)
    ## move brush?
    .bmove = TRUE

		## layer opacity
		.alpha = alpha

    ## the title string
    dataname = deparse(substitute(data))
    if (missing(main)) {
        main = paste("Parallel Coordinates Plot of", dataname)
    }

    ## variable selection: select all variables not with '.' prefix by default;
    ##   or parse the formula to get var names; or just use the usual way to
    ##   select vars, e.g. by colnames, or integers
    if (missing(vars)) vars = grep('^[^.]', names(data), value = TRUE)
    if (class(vars) == "formula")
        vars = attr(terms(vars, data = as.data.frame(data)), "term.labels")
    if (is.numeric(vars)) vars = names(data)[as.integer(vars)]

    if (length(vars) <= 1L)
        stop("parallel coordinate plots need at least 2 variables!")

    glyph = match.arg(glyph)
    order = match.arg(order)

    ## a long way of transformation
    ## creat some 'global' variables first
    x = y = n = nn = numcol = p = segx0 = segy0 = segx1 = segy1 = segcol =
        xr = yr = xspan = yspan = xticklab = yticklab = xtickloc = ytickloc =
            .brange = lims = x0 = y0 = plot_data = bxpstats = NULL
    .drawrange = TRUE

    data_preprocess = function() {

        ## get the plotting data: we don't want to change the original mutaframe
        ##   so get an independent copy here
        plot_data <<- as.data.frame(data[, vars], stringsAsFactors = TRUE)

        ## constant columns (or nearly constants -- for safety with floating numbers)
        const.col = sapply(plot_data, function(x) {
            x = na.omit(x)
            x = as.numeric(x)
            length(x) == 0 || diff(range(x)) < 1e-6
        })
        ## remove constant columns and give a warning message
        if (any(const.col)) {
            plot_data <<- plot_data[, !const.col]
            warning("removed constant column(s) ",
                    paste(vars[const.col], collapse = ","))
            vars <<- vars[!const.col]
        }

        ## which columns are numeric? we don't want boxplots for non-numeric vars
        numcol <<- sapply(plot_data, class) %in% c("numeric", "integer")

        plot_data <<- sapply(plot_data, as.numeric)
        ## must make them global; I wish there could be 'mutavectors'
        p <<- ncol(plot_data)
        n <<- nrow(plot_data)

        ## handle missing values
        plot_data <<- na.action(plot_data)

        ## jittering
        if (!is.null(jitter)) {
            if (class(jitter) == "formula")
                jitter = attr(terms(jitter, data = plot_data), "term.labels")
            if (is.numeric(jitter)) jitter = names(data)[as.integer(jitter)]
            if (is.character(jitter)) {
                plot_data[, jitter] <<- apply(plot_data[, jitter, drop = FALSE],
                                              2, base::jitter, amount = amount)
            }
        }

        ## standardizing
        scale = switch(scale, range = function(x) {
            xna = x[!is.na(x)]
            (x - min(xna))/(max(xna) - min(xna))
        }, var = base:::scale, I = identity, get(scale))
        plot_data <<- apply(plot_data, 2, scale)

        ## centering
        if (!is.null(center)){
            plot_data <<- apply(plot_data, 2, function(xx) {
                xx - ifelse(is.function(center), center(xx), as.numeric(center))
            })
        }
    }

    ## flip the variables

    ## final calculations for graphical primitives, e.g. segments, axis labels
    data_primitives = function() {

        ## switch x and y according to the direction
        if (horizontal) {
            x <<- col(plot_data)
            y <<- plot_data
            xtickloc <<- 1:p
            xticklab <<- vars
            ytickloc <<- pretty(y)
            yticklab <<- format(ytickloc)
        }
        else {
            x <<- plot_data
            y <<- col(plot_data)
            xtickloc <<- pretty(x)
            xticklab <<- format(xtickloc)
            ytickloc <<- 1:p
            yticklab <<- vars
        }
        xspan <<- range(x)
        yspan <<- range(y)
        xr <<- diff(xspan)
        yr <<- diff(yspan)

        ## margins for the plot region
        mar = qpar('mar')
        lims <<- matrix(c(xspan + c(-1, 1) * xr * mar,
                          yspan + c(-1, 1) * yr * mar), 2)
        ## adjust axis ticks locations (some may exceed the range of 'lims')
        idx = (xtickloc > lims[1, 1]) & (xtickloc < lims[2, 1])
        xtickloc <<- xtickloc[idx]
        xticklab <<- xticklab[idx]
        idx = (ytickloc > lims[1, 2]) & (ytickloc < lims[2, 2])
        ytickloc <<- ytickloc[idx]
        yticklab <<- yticklab[idx]
        if (!is.null(lab.split)) {
            if (horizontal) {
                xticklab <<- gsub(lab.split, '\n', xticklab)
            } else {
                yticklab <<- gsub(lab.split, '\n', yticklab)
            }
        }

        ## 'auto' means 'line's when n*p<=5000*10, and 'tick's otherwise
        if (glyph == 'auto') glyph <<- ifelse(n * p <= 50000, 'line', 'tick')
        if (glyph == 'line') {
            ## creating starting and ending vectors, because indexing in real-time can be slow
            segx0 <<- as.vector(t.default(x[, 1:(p - 1)]))
            segx1 <<- as.vector(t.default(x[, 2:p]))
            segy0 <<- as.vector(t.default(y[, 1:(p - 1)]))
            segy1 <<- as.vector(t.default(y[, 2:p]))
            nn <<- n * (p - 1)
        } else {
            x0 <<- as.vector(t.default(x))
            y0 <<- as.vector(t.default(y))
        }

    }

    data_boxplot = function() {
        ## for boxplots
        if (boxplot)
            bxpstats <<- apply(plot_data, 2,
                               function(x) boxplot.stats(x, do.conf = FALSE)$stats)
    }

    ## given orders, rearrange the data
    ## need to update: numcol, plot_data, vars, boxplot data, primitives data
    data_reorder = function(vars) {
        numcol = numcol
        names(numcol) = colnames(plot_data)
        numcol <<- numcol[vars]
        plot_data <<- plot_data[, vars]
        vars <<- colnames(plot_data)
        data_boxplot()
        data_primitives()
    }

    ## do the transformation now
    data_preprocess()

    ## order by MDS or ANOVA
    data_reorder(reorder_var(data = plot_data, type = order, vars = vars,
                             numcol = numcol, x = data$.color))

    ## brush range: horizontal and vertical
    .brange = c(xr, yr)/30

    ## automatic box width
    if (missing(boxwex))
        boxwex = max(1/p, 0.2)

    ## convention of notation:
    ## *_draw means a drawing function for a layer; *_event is an even callback; *_layer is a layer object

    draw.glyph = switch(glyph, tick = qglyphSegment(b = ifelse(horizontal, 0, Inf)), circle = qglyphCircle(),
        square = qglyphSquare(), triangle = qglyphTriangle())

    ## par-coords segments
    main_draw = function(layer, painter) {
        cranvas_debug()
        .color = data$.color
        .color[!visible(data)] = NA
        layer$setOpacity(.alpha)
        if (glyph == 'line') {
            segcol = rep(.color, each = p - 1)
            qdrawSegment(painter, segx0, segy0, segx1, segy1, stroke = segcol)
        } else {
            col.glyph = rep(.color, each = p)
            qdrawGlyph(painter, draw.glyph, x0, y0, stroke = col.glyph)
        }
        cranvas_debug()
    }

    ## annotate maximum and minimum values for each axis
    range_draw = function(layer, painter) {
        if (any(numcol)) {
            dat = as.data.frame(data)[, vars][, numcol]
            if (!.drawrange) return()
            range.d = round(as.matrix(apply(dat, 2, range, na.rm=TRUE)), 2)
            qstrokeColor(painter) = data$.color[1]
            if (horizontal) {
                qdrawText(painter, range.d[1, ], which(numcol), lims[3], valign = 'bottom')
                qdrawText(painter, range.d[2, ], which(numcol), lims[4], valign = 'top')
            } else {
                qdrawText(painter, range.d[1, ], lims[1], which(numcol), halign = 'left')
                qdrawText(painter, range.d[2, ], lims[2], which(numcol), halign = 'right')
            }
        }
    }

    ## (optional) boxplots
    boxplot_draw = function(layer, painter) {
        cranvas_debug()
        qstrokeColor(painter) = "black"
        for (i in (1:p)[numcol]) {
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
        .bstart <<- as.numeric(event$pos())
        ## on right click, we can resize the brush; left click: only move the brush
        if (event$button() == Qt$Qt$RightButton) {
            .bmove <<- FALSE
        }
        if (event$button() == Qt$Qt$LeftButton) {
            .bmove <<- TRUE
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
            .alpha <<- min(c(1.1, .9)[i] * .alpha, 1)
            main_layer$setOpacity(.alpha)
            qupdate(main_layer)
        }

        ## whether to draw min/max labels
        if (key == Qt$Qt$Key_R) {
            .drawrange <<- !.drawrange
            qupdate(range_layer)
            return()
        }

        ## make the brushed observations invisible when hitting Delete
        if (key == Qt$Qt$Key_Delete)
            visible(data) = !selected(data)
        i = which(key == c(Qt$Qt$Key_Left, Qt$Qt$Key_Right, Qt$Qt$Key_Down, Qt$Qt$Key_Up))
        if (length(i) && !any(is.na(.bpos))) {
            if (horizontal) {
                j = 1
                movedir = switch(i, -1, 1, NULL, NULL)
                flipdir = switch(i, NULL, NULL, -1, 1)
            } else {
                j = 2
                movedir = switch(i, NULL, NULL, -1, 1)
                flipdir = switch(i, -1, 1, NULL, NULL)
            }
            xs = which((1:p) > .bpos[j] - .brange[j] & (1:p) < .bpos[j] + .brange[j])
            if ((nxs <- length(xs))) {
                if (!is.null(movedir)) {
                    vars0 = vars
                    if (xs[1] > 1 & movedir == -1){
                        vars0[c(xs[1] - 1, xs)] = vars0[c(xs, xs[1] - 1)]
                        .bpos[j] <<- .bpos[j] - 1
                    }
                    if (xs[nxs] < p & movedir == 1){
                        vars0[c(xs, xs[nxs] + 1)] = vars0[c(xs[nxs] + 1, xs)]
                        .bpos[j] <<- .bpos[j] + 1
                    }
                    if (any(vars0 != vars)) {
                        data_reorder(vars0)
                        qupdate(xaxis_layer)
                        qupdate(yaxis_layer)
                        main_layer$invalidateIndex()
                        qupdate(main_layer)
                        qupdate(brush_layer)
                        if (boxplot) {
                            data_boxplot()
                            qupdate(boxplot_layer)
                        }
                    }
                }
                if (!is.null(flipdir)) {
                    plot_data[, xs] <<- apply(plot_data[, xs, drop = FALSE], 2,
                                              function(xx) {
                                                  lims[c(3,1)[j]] + lims[c(4,2)[j]] - xx
                                              })
                    data_primitives()
                    qupdate(xaxis_layer)
                    qupdate(yaxis_layer)
                    main_layer$invalidateIndex()
                    qupdate(main_layer)
                    qupdate(brush_layer)
                    if (boxplot) {
                        data_boxplot()
                        qupdate(boxplot_layer)
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
            .brushed = logical(n)
            .brushed[b$history.list[[idx]]] = TRUE
            selected(data) = .brushed
        }
    }

    ## identify segments being brushed when the mouse is moving
    brush_mouse_move = function(layer, event) {
        cranvas_debug()
        if (b$identify) return()
        .bpos <<- as.numeric(event$pos())
        ## simple click: don't change .brange
        if (!all(.bpos == .bstart) && (!.bmove)) {
            .brange <<- .bpos - .bstart
        }
        .new.brushed = rep(FALSE, n)
        rect = qrect(matrix(c(.bpos - .brange, .bpos + .brange), 2, byrow = TRUE))
        hits = layer$locate(rect) + 1
        ## ticks and lines are of different numbers!
        hits = ceiling(hits/ifelse(glyph == 'line', p - 1, p))
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
        if (!any(is.na(.bpos))) {
            qlineWidth(painter) = b$style$size
            ##qdash(painter)=c(1,3,1,3)
            qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - .brange[2],
                      .bpos[1] + .brange[1], .bpos[2] + .brange[2],
                      stroke = b$style$color)
        }
        if (b$persistent && length(b$persistent.list)) {
            qlineWidth(painter) = b$size
            for (i in seq_along(b$persistent.list)) {
                idx = b$persistent.list[[i]]
                qstrokeColor(painter) = b$persistent.color[i]
                tmpx = mat2seg(x, idx)
                tmpy = mat2seg(y, idx)
                nn = length(tmpx)
                qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
            }
        }
        .brushed = selected(data)
        if (sum(.brushed, na.rm = TRUE) >= 1) {
            qlineWidth(painter) = b$size
            qstrokeColor(painter) = b$color
            tmpx = mat2seg(x, .brushed)
            tmpy = mat2seg(y, .brushed)
            nn = length(tmpx)
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])

       }
        cranvas_debug()
    }

    .identified = NULL
    identify_hover = function(layer, event) {
        if (!b$identify) return()
        .bpos <<- as.numeric(event$pos())
        rect = qrect(matrix(c(.bpos - c(xr, yr)/100, .bpos + c(xr, yr)/100), 2, byrow = TRUE))
        hits = layer$locate(rect) + 1
        .identified <<- ceiling(hits/ifelse(glyph == 'line', p - 1, p))
        qupdate(identify_layer)
    }

    identify_draw = function(layer, painter) {
        if (b$identify && length(.identified)) {
            qlineWidth(painter) = b$size
            qstrokeColor(painter) = b$color
            tmpx = mat2seg(x, .identified)
            tmpy = mat2seg(y, .identified)
            nn = length(tmpx)
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])

            .labels = b$label.gen(data[.identified, vars])
            bgwidth = qstrWidth(painter, .labels)
            bgheight = qstrHeight(painter, .labels)
            ## adjust drawing directions when close to the boundary
            hflag = lims[2] - .bpos[1] > bgwidth
            vflag = .bpos[2] - lims[3] > bgheight
            qdrawRect(painter, .bpos[1], .bpos[2],
                      .bpos[1] + ifelse(hflag, 1, -1) * bgwidth,
                      .bpos[2] + ifelse(vflag, -1, 1) * bgheight,
                      stroke = rgb(1, 1, 1, 0.8), fill = rgb(1, 1, 1, 0.8))
            qstrokeColor(painter) = b$label.color
            qdrawText(painter, .labels, .bpos[1], .bpos[2],
                      halign = ifelse(hflag, "left", "right"),
                      valign = ifelse(vflag, "top", "bottom"))
        }
    }

    scene = qscene()
    root_layer = qlayer(scene)

    ## title
    title_layer = qlayer(root_layer, function(layer, painter) {
        qdrawText(painter, main, (lims[1] + lims[2])/2, 0, "center", "bottom")
    }, limits = qrect(c(lims[1], lims[2]), c(0, 1)), row = 0, col = 1)
    ## x and y-axis
    if (horizontal) {
        xat = 1:p
        yat = .axis.loc(y)
    } else {
        xat = .axis.loc(x)
        yat = 1:p
    }
    xaxis_layer = qaxis(at = xat, labels = xticklab, side = 1, limits = lims[1:2])
    yaxis_layer = qaxis(at = yat, labels = yticklab, side = 2, limits = lims[3:4])
    grid_layer = qgrid(xat = xat, yat = yat, xlim = lims[1:2], ylim = lims[3:4])
    root_layer[2, 1] = xaxis_layer
    root_layer[1, 0] = yaxis_layer
    root_layer[1, 1] = grid_layer

    if (boxplot) {
        boxplot_layer = qlayer(root_layer, boxplot_draw, limits = qrect(lims),
        row = 1, col = 1)
    }

    main_layer = qlayer(root_layer, main_draw,
        mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_move,
        mouseMove = brush_mouse_move, keyPressFun = brush_key_press,
        keyReleaseFun = brush_key_release, hoverMoveFun = identify_hover,
        focusInFun = function(layer, painter) {
            focused(data) = TRUE
        }, focusOutFun = function(layer, painter) {
            focused(data) = FALSE
        },
        limits = qrect(lims), row = 1, col = 1)

    range_layer = qlayer(root_layer, range_draw, limits = qrect(lims), row = 1, col = 1)
    brush_layer = qlayer(root_layer, brush_draw, limits = qrect(lims), row = 1, col = 1)
    identify_layer = qlayer(root_layer, identify_draw, limits = qrect(lims), row = 1, col = 1)
    ## legend layer (currently only acts as place holder)
    legend_layer = qlayer(root_layer, row = 1, col = 2)

    ## update the brush layer in case of any modifications to the mutaframe
    d.idx = add_listener(data, function(i, j) {
        switch(j, .brushed = qupdate(brush_layer),
               .color = qupdate(main_layer), {
                   data_preprocess()
                   data_primitives()
                   qupdate(grid_layer)
                   qupdate(xaxis_layer)
                   qupdate(yaxis_layer)
                   qupdate(main_layer)
                   if (boxplot) {
                       data_boxplot()
                       qupdate(boxplot_layer)
                   }
               })
    })

    ## update the brush layer if brush attributes change
    brush_update = function() {
        qupdate(brush_layer)
    }
    b.idx = b$colorChanged$connect(brush_update)
    qconnect(main_layer, 'destroyed', function(x) {
        b$colorChanged$disconnect(b.idx)
        remove_listener(data, d.idx)
    })
    ## more attributes to come

    layout = root_layer$gridLayout()
    layout$setRowPreferredHeight(0, 30)
    ## the y-axis layer needs 'dynamic' width determined by #{characters}
    ## here is a formula by my rule of thumb: 9 * nchar + 5
    layout$setColumnPreferredWidth(0, 9 * max(nchar(unlist(strsplit(yticklab, '\n')))) + 5)
    layout$setRowPreferredHeight(2, 15 * max(sapply(gregexpr('\\n', xticklab),
                              function(xx) ifelse(any(xx <0), 0, length(xx)) + 2)))
    layout$setColumnMaximumWidth(2, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setColumnStretchFactor(0, 0)
    layout$setRowStretchFactor(2, 0)

    view = qplotView(scene = scene)
    view$setWindowTitle(main)
    view
}
