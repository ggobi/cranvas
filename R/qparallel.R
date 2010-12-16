##' Parallel coordinates plot.
##' Create a parallel coordinates plot from a data frame or matrix, with each
##' line representing a row.
##'
##' @param data a mutaframe which is typically built upon a data frame
##' along with several row attributes
##' @param vars variables to show; can be a character vector (column
##' names), an integer vector (column indices) or a formula like '~ x1
##' + x2'
##' @param scale standardizing method - 'range' --> [0, 1], 'I' --> do
##' nothing, 'var' --> mean 0 var 1, 'custom_function_name' --> use
##' your own function (see examples.R)
##' @param na.action the function to deal with missing values
##' @param center the function to calculate where to center all the
##' axes (e.g. center at the medians), or a numeric value, or
##' \code{NULL} (do not center)
##' @param order for \code{order = 'MDS'}, reorder the variables by
##' classical multidimensional scaling so that similar variables will
##' be arranged nearer to each other (categorical variables will be
##' put to the last); for \code{order = 'ANOVA'}, reorder the
##' variables by the p-values associated with the ANOVA based on each
##' variable versus the \code{data$.color} variable, so that the
##' variable with largest between-group difference will be put in the
##' first place, and so on; \code{order = 'none'} means keep the
##' original order.
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
##' @param mar margin (in proportion to the whole canvas)
##' @param main the title
##' @param lab.split the pattern to ``break'' the axis labels by
##' \code{'\n'}; the default \code{'[^[:alnum:]]'} means any
##' characters which are not alphanumeric will be replaced by
##' \code{'\n'}, i.e. the labels will be broken into several lines;
##' this can be useful when the axis labels are too long (if we do not
##' break them, they will be squeezed together along the axes)
##' @param verbose print some extra information (mainly the time
##' consumed in each step); set \code{lab.split = NULL} to keep the
##' labels untouched
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example cranvas/inst/examples/qparallel-ex.R
qparallel = function(data, vars, scale = "range", na.action = na.impute,
    center = NULL, order = c('none', 'MDS', 'ANOVA'), horizontal = TRUE,
    glyph = c('auto', 'line', 'tick', 'circle', 'square', 'triangle'),
    boxplot = FALSE, boxwex, jitter = NULL, amount = NULL,
    mar = c(0.04, 0.04, 0.04, 0.04), main, lab.split = '[^[:alnum:]]',
    verbose = getOption("verbose")) {

    ## parameters for the brush
    .brush.attr = brush_attr(data)
    ## background color
    .bgcolor = "grey80"
    ## .bgcolor = rgb(0,0,0,0)
    ## mouse position
    .bpos = c(NA, NA)
    ## drag start
    .bstart = c(NA, NA)
    ## move brush?
    .bmove = TRUE

    ## check if an attribute exists
    has_attr = function(attr) {
        attr %in% names(data)
    }

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
        stop("parallel cooridinates plots need at least 2 variables!")

    ## obtain colors from the original mutaframe
    if (!has_attr('.color')) data$.color = 'black'
    ## brushed
    if (!has_attr('.brushed')) data$.brushed = FALSE

    glyph = match.arg(glyph)
    order = match.arg(order)

    ## a long way of transformation
    ## creat some 'global' variables first
    x = y = n = nn = numcol = p = segx0 = segy0 = segx1 = segy1 = segcol =
        xr = yr = xspan = yspan = xticklab = yticklab = xtickloc = ytickloc =
            .brange = lims = x0 = y0 = plot_data = bxpstats = NULL

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

    data_calc_order = function() {
        ## ordering variables by MDS or ANOVA
        if (any(numcol)) {
            num_data = plot_data[, numcol, drop = FALSE]
            switch(order, none = NULL, MDS = {
                idx = order(cmdscale(1 - cor(num_data), k = 1))
            }, ANOVA = {
                if (length(unique(data$.color)) > 1) {
                    xfactor = factor(data$.color)
                    idx = order(apply(num_data, 2, function(y) {
                        summary(aov(y ~ xfactor))[[1]][1, 5]
                    }))
                } else {
                    idx=1:ncol(num_data)
                }
            })
            if (order != 'none') {
                return(c(vars[numcol][idx], vars[!numcol]))
            }
        }
        vars
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
        mar = rep(mar, length.out = 4)
        lims <<- matrix(c(xspan + c(-1, 1) * xr * mar[c(2, 4)],
                          yspan + c(-1, 1) * yr * mar[c(1, 3)]), 2)
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
    data_reorder(data_calc_order())

    ## brush range: horizontal and vertical
    .brange = c(xr, yr)/30

    ## automatic box width
    if (missing(boxwex))
        boxwex = max(1/p, 0.2)

    ## convention of notation:
    ## *_draw means a drawing function for a layer; *_event is an even callback; *_layer is a layer object

    draw.glyph = switch(glyph, tick = qglyphSegment(b = ifelse(horizontal, 0, Inf)), circle = qglyphCircle(),
        square = qglyphSquare(), triangle = qglyphTriangle())

    ## background grid
    grid_draw = function(item, painter) {
        qdrawRect(painter, lims[1, 1], lims[1, 2], lims[2, 1], lims[2, 2],
                  stroke = .bgcolor, fill = .bgcolor)
        qdrawSegment(painter, xtickloc, lims[1, 2], xtickloc, lims[2, 2],
                     stroke = "white")
        qdrawSegment(painter, lims[1, 1], ytickloc, lims[2, 1], ytickloc,
                     stroke = "white")
    }

    ## par-coords segments
    main_draw = function(item, painter) {
        if (verbose) {
            ntime = Sys.time()
            message("drawing pcp segments")
        }
        if (glyph == 'line') {
            segcol = rep(data$.color, each = p - 1)
            qdrawSegment(painter, segx0, segy0, segx1, segy1, stroke = segcol)
        } else {
            col.glyph = rep(data$.color, each = p)
            qdrawGlyph(painter, draw.glyph, x0, y0, stroke = col.glyph)
        }
        if (verbose)
            message(format(difftime(Sys.time(), ntime)))
    }

    ## annotate maximum and minimum values for each axis
    range_draw = function(item, painter) {
        if (any(numcol)) {
            dat = as.data.frame(data)[, vars][, numcol]
            if (horizontal) {
                qdrawText(painter, apply(dat, 2, min, na.rm=TRUE), which(numcol), yspan[1], valign = 'top')
                qdrawText(painter, apply(dat, 2, max, na.rm=TRUE), which(numcol), yspan[2], valign = 'bottom')
            } else {
                qdrawText(painter, apply(dat, 2, min, na.rm=TRUE), xspan[1], which(numcol), halign = 'right')
                qdrawText(painter, apply(dat, 2, max, na.rm=TRUE), xspan[2], which(numcol), halign = 'left')
            }
        }
    }

    ## (optional) boxplots
    boxplot_draw = function(item, painter) {
        if (verbose) {
            message("Drawing boxplots")
            ntime = Sys.time()
        }
        qstrokeColor(painter) = "black"
        for (i in (1:p)[numcol]) {
            x0 = c(rep(i - boxwex/2, 6), i + boxwex/2, rep(i, 2))
            y0 = c(bxpstats[, i], rep(bxpstats[2, i], 2), bxpstats[c(1, 4), i])
            x1 = c(rep(i + boxwex/2, 5), i - boxwex/2, i + boxwex/2, rep(i, 2))
            y1 = c(bxpstats[, i], rep(bxpstats[4, i], 2), bxpstats[c(2, 5), i])
            if (horizontal) {
                qdrawSegment(painter, x0, y0, x1, y1)
                qlineWidth(painter) = 3
                qdrawSegment(painter, x0[3], y0[3], x1[3], y1[3])
                qlineWidth(painter) = 1
            }
            else {
                qdrawSegment(painter, y0, x0, y1, x1)
                qlineWidth(painter) = 3
                qdrawSegment(painter, y0[3], x0[3], y1[3], x1[3])
                qlineWidth(painter) = 1
            }
        }
        if (verbose)
            message(format(difftime(Sys.time(), ntime)))
    }

    ## record the coordinates of the mouse on click
    brush_mouse_press = function(item, event) {
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
    identify_key_press = function(layer, event) {
        ## Key X: XOR; O: OR; A: AND; N: NOT
        i = which(event$key() == c(Qt$Qt$Key_A, Qt$Qt$Key_O, Qt$Qt$Key_X, Qt$Qt$Key_N, Qt$Qt$Key_C))
        if (length(i))
            brush_attr(data, '.brush.mode') = c('and', 'or', 'xor', 'not', 'complement')[i]
        i = which(event$key() == c(Qt$Qt$Key_Left, Qt$Qt$Key_Right, Qt$Qt$Key_Down, Qt$Qt$Key_Up))
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
    identify_key_release = function(layer, event) {
        ## set brush mode to 'none' when release the key
        brush_attr(data, '.brush.mode') = 'none'
        direction = which(event$key() == c(Qt$Qt$Key_PageUp, Qt$Qt$Key_PageDown))
        if (length(direction)) {
            .brush.index = .brush.attr$.brush.index + c(-1, 1)[direction]
            .brush.index = max(1, min(length(.brush.attr$.brush.history), .brush.index))
            .brush.attr$.brush.index = .brush.index
            .brushed = logical(n)
            .brushed[.brush.attr$.brush.history[[.brush.index]]] = TRUE
            data$.brushed = .brushed
        }
    }

    ## identify segments being brushed when the mouse is moving
    identify_mouse_move = function(layer, event) {
        if (verbose) {
            message("identifying mouse location and looking for segments within range")
            ntime = Sys.time()
        }
        pos = event$pos()
        .bpos <<- as.numeric(pos)
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
        data$.brushed = mode_selection(data$.brushed, .new.brushed, mode = brush_attr(data, '.brush.mode'))
        ## on mouse release
        if (event$button() != Qt$Qt$NoButton) {
            .brush.attr$.brush.history[[(csize <- length(.brush.attr$.brush.history) + 1)]] = which(data$.brushed)
            ## remove the first few columns due to the hisotory size limit
            if (csize > (hsize <- brush_attr(data, '.history.size'))) {
                .brush.attr$.brush.history[1:(csize - hsize)] = NULL
            }
            .brush.attr$.brush.index = length(.brush.attr$.brush.history)
        }
        if (verbose)
            message(format(difftime(Sys.time(), ntime)))
    }

    ## draw the segments under the brush with another appearance
    brush_draw = function(item, painter) {
        if (verbose) {
            message("drawing brushed segments")
            ntime = Sys.time()
        }

        if (!any(is.na(.bpos))) {
            qlineWidth(painter) = brush_attr(data, '.brush.size')
            ##qdash(painter)=c(1,3,1,3)
            qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - .brange[2],
                      .bpos[1] + .brange[1], .bpos[2] + .brange[2],
                      stroke = brush_attr(data, '.brush.color'))
        }
        .brushed = data$.brushed
        if (sum(.brushed, na.rm = TRUE) >= 1) {
            qlineWidth(painter) = brush_attr(data, '.brushed.size')
            qstrokeColor(painter) = brush_attr(data, '.brushed.color')
            x = x[.brushed, , drop = FALSE]
            y = y[.brushed, , drop = FALSE]
            tmpx = as.vector(t.default(cbind(x, NA)))
            tmpy = as.vector(t.default(cbind(y, NA)))
            nn = length(tmpx)
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])

            ## show data labels and row ids
            if (brush_attr(data, '.label.show') && !any(is.na(.bpos))) {
                ## retrieve labels from the original data (possibly w/ transformation)
                .label.fun = brush_attr(data, '.label.fun')
                .brush.labels = .label.fun(data[.brushed, vars])
                .vars = c('case id', vars)
                ## truncate the id strings if too long
                .caseid = ifelse(sum(.brushed) == 1, rownames(data)[.brushed],
                    truncate_str(paste(rownames(data)[.brushed], collapse = ', '),
                                 max(nchar(c(.brush.labels, .vars)))))
                .brush.labels = c(.caseid, .brush.labels)
                .brush.labels = paste(.vars, .brush.labels, sep = ': ', collapse = '\n')
                bgwidth = qstrWidth(painter, .brush.labels)
                bgheight = qstrHeight(painter, .brush.labels)
                ## adjust drawing directions when close to the boundary
                hflag = lims[2] - .bpos[1] > bgwidth
                vflag = .bpos[2] - lims[3] > bgheight
                qdrawRect(painter, .bpos[1], .bpos[2],
                          .bpos[1] + ifelse(hflag, 1, -1) * bgwidth,
                          .bpos[2] + ifelse(vflag, -1, 1) * bgheight,
                          stroke = rgb(1, 1, 1, 0.5), fill = rgb(1, 1, 1, 0.5))
                qstrokeColor(painter) = brush_attr(data, '.label.color')
                qdrawText(painter, .brush.labels, .bpos[1], .bpos[2],
                          halign = ifelse(hflag, "left", "right"),
                          valign = ifelse(vflag, "top", "bottom"))
            }
        }
        if (verbose)
            message(format(difftime(Sys.time(), ntime)))
    }

    scene = qscene()
    root_layer = qlayer(scene)
    ## title
    title_layer = qlayer(root_layer, function(item, painter) {
        qdrawText(painter, main, (lims[1] + lims[2])/2, 0, "center", "bottom")
    }, limits = qrect(c(lims[1], lims[2]), c(0, 1)), row = 0, col = 1)
    ## y-axis
    yaxis_layer = qlayer(root_layer, function(item, painter) {
        qdrawText(painter, yticklab, 0.9, ytickloc, "right", "center")
        qdrawSegment(painter, .91, ytickloc, 1, ytickloc, stroke='black')
    }, limits = qrect(c(0, 1), c(lims[3], lims[4])), row = 1, col = 0)
    ## x-axis
    xaxis_layer = qlayer(root_layer, function(item, painter) {
        qdrawText(painter, xticklab, xtickloc, 0.9, "center", "top")
        qdrawSegment(painter, xtickloc, .91, xtickloc, 1, stroke='black')
    }, limits = qrect(c(lims[1], lims[2]), c(0, 1)), row = 2, col = 1)

    grid_layer = qlayer(root_layer, grid_draw, limits = qrect(lims), row = 1, col = 1)

    main_layer = qlayer(root_layer, main_draw,
        mousePressFun = brush_mouse_press, mouseReleaseFun = identify_mouse_move,
        mouseMove = identify_mouse_move, keyPressFun = identify_key_press,
        keyReleaseFun = identify_key_release,
        limits = qrect(lims), row = 1, col = 1)

    range_layer = qlayer(root_layer, range_draw, limits = qrect(lims), row = 1, col = 1)
    if (boxplot) {
        boxplot_layer = qlayer(root_layer, boxplot_draw, limits = qrect(lims),
        row = 1, col = 1)
    }
    brush_layer = qlayer(root_layer, brush_draw, limits = qrect(lims),
        row = 1, col = 1)
    ## legend layer (currently only acts as place holder)
    legend_layer = qlayer(root_layer, row = 1, col = 2)

    ## update the brush layer in case of any modifications to the mutaframe
    add_listener(data, function(i, j) {
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
    add_listener(.brush.attr, function(i, j) {
        qupdate(brush_layer)
    })

    layout = root_layer$gridLayout()
    layout$setRowMaximumHeight(0, 30)
    ## the y-axis layer needs 'dynamic' width determined by #{characters}
    ## here is a formula by my rule of thumb: 9 * nchar + 5
    layout$setColumnMaximumWidth(0, 9 * max(nchar(unlist(strsplit(yticklab, '\n')))) + 5)
    layout$setRowMaximumHeight(2, 15 * max(sapply(gregexpr('\\n', xticklab),
                              function(xx) ifelse(any(xx <0), 0, length(xx)) + 2)))
    layout$setColumnMaximumWidth(2, 10)

    view = qplotView(scene = scene)
    view$setWindowTitle(main)
    view
}
