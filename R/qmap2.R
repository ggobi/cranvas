##' Draw a map
##'
##' This function creates a map based on the data of polygon
##' coordinates.
##'
##' Common interactions are documented in
##' \code{\link{common_key_press}}. Currently the brushing may be
##' slightly different with other plots: when a region contains
##' multiple sub-regions (polygons), and the whole region will be
##' brushed if any of the sub-regions is brushed.
##'
##' The other feature is that a map can be linked to other datasets so
##' that colors and the brushed status can be obtained from an
##' additional data. This is useful due to the special format of map
##' data: usually it is a waste of memory to store colors for every
##' single boundary points in each region.
##' @param data the map data created by \code{\link{map_qdata}}
##' @param linkto a mutaframe to link to so that the colors and the
##' brushed status are in sync with this data
##' @param linkby the variable in the \code{linkto} data to be used as
##' a linking variable (see \code{\link{link_cat}})
##' @inheritParams qbar
##' @return A map
##' @export
##' @author Heike Hofmann and Yihui Xie
##' @example inst/examples/qmap2-ex.R
qmap2 =
    function(data = last_data(), linkto = NULL, linkby = NULL,
             main = '', xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL) {
    data = check_data(data)
    if (is.null(md <- attr(data, 'MapData')))
        stop('data must be created from map_qdata()')
    b = brush(data)
    z = as.list(match.call()[-1])

    ## initialize meta
    meta =
        Map.meta$new(alpha = 1, main = main, minor = 'xy',
                     group = cumsum(is.na(md$x) & is.na(md$y)) + 1)

    ## set default xlab/ylab if not provided
    if (is.null(xlab)) meta$xlab = 'longitude'
    if (is.null(ylab)) meta$ylab = 'latitude'

    ## compute coordinates/axes-related stuff
    compute_coords = function() {
        meta$xat = axis_loc(md$x); meta$yat = axis_loc(md$y)
        meta$xlabels = format(meta$xat); meta$ylabels = format(meta$yat)
        r =
            cbind(if (is.null(xlim))
                  range(md$x, na.rm = TRUE, finite = TRUE) else xlim,
                  if (is.null(ylim))
                  range(md$y, na.rm = TRUE, finite = TRUE) else ylim)
        meta$limits = extend_ranges(r)
    }
    compute_coords()

    compute_colors = function() {
        meta$border = data$.border
        if (is.null(linkto)) {
            meta$color = data$.color
        } else {
            if (is.null(linkby)) stop("must specify a linking variable in 'linkto'")
            tmp = tapply(linkto$.color, linkto[, as.character(z$linkby)], `[`, 1)
            meta$color = tmp[data$labels]  # use labels to find colors
        }
    }
    compute_colors()

    ## initialize brush size (1/15 of the layer size)
    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15

    ## draw points
    main_draw = function(layer, painter) {
        qdrawPolygon(painter, md$x, md$y, stroke = meta$border, fill = meta$color)
    }

    ## draw brushed points
    brush_draw = function(layer, painter) {
        if (b$identify) return()
        idx = selected(data)
        if (any(idx)) {
            i = meta$group %in% which(idx)
            qdrawPolygon(painter, md$x[i], md$y[i], stroke = NA, fill = b$color)
        }
        draw_brush(layer, painter, data, meta)
    }

    ## events
    brush_mouse_press = function(layer, event) {
        common_mouse_press(layer, event, data, meta)
    }
    brush_mouse_move = function(layer, event) {
        rect = qrect(update_brush_size(meta, event))
        hits = layer$locate(rect) + 1
        if (length(hits)) {
            hits = data$labels %in% unique(data$labels[hits])
        }
        selected(data) = mode_selection(selected(data), hits, mode = b$mode)
        common_mouse_move(layer, event, data, meta)
    }
    brush_mouse_release = function(layer, event) {
        brush_mouse_move(layer, event)
        common_mouse_release(layer, event, data, meta)
    }
    key_press = function(layer, event) {
        common_key_press(layer, event, data, meta)
    }
    key_release = function(layer, event) {
        common_key_release(layer, event, data, meta)
    }
    mouse_wheel = function(layer, event) {
        meta$limits = extend_ranges(meta$limits, -sign(event$delta()) * 0.05)
    }
    identify_hover = function(layer, event) {
        if (!b$identify) return()
        b$cursor = 2L
        meta$pos = as.numeric(event$pos())
        hits = layer$locate(identify_rect(meta)) + 1
        if (length(hits)) {
            hits = which(data$labels %in% unique(data$labels[hits]))
        }
        meta$identified = hits
        qupdate(layer.identify)
    }
    identify_draw = function(layer, painter) {
        if (!b$identify || !length(idx <- meta$identified)) return()
        if (any(idx)) {
            meta$identify.labels = paste(unique(data$labels[idx]), collapse = '\n')
            draw_identify(layer, painter, data, meta)
            i = meta$group %in% idx
            qdrawPolygon(painter, md$x[i], md$y[i], stroke = b$color, fill = NA)
        }
    }

    ## create layers
    scene = qscene()
    layer.root = qlayer(scene)
    layer.main =
        qlayer(paintFun = main_draw,
               mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_release,
               mouseMove = brush_mouse_move, hoverMoveFun = identify_hover,
               keyPressFun = key_press, keyReleaseFun = key_release,
               wheelFun = mouse_wheel,
               limits = qrect(meta$limits), clip = TRUE)
    layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
    layer.identify = qlayer(paintFun = identify_draw, limits = qrect(meta$limits))
    layer.title = qmtext(meta = meta, side = 3)
    layer.xlab = qmtext(meta = meta, side = 1)
    layer.ylab = qmtext(meta = meta, side = 2)
    layer.xaxis = qaxis(meta = meta, side = 1)
    layer.yaxis = qaxis(meta = meta, side = 2)
    layer.grid = qgrid(meta = meta)
    layer.root[0, 2] = layer.title
    layer.root[2, 2] = layer.xaxis
    layer.root[3, 2] = layer.xlab
    layer.root[1, 1] = layer.yaxis
    layer.root[1, 0] = layer.ylab
    layer.root[1, 2] = layer.grid
    layer.root[1, 2] = layer.main
    layer.root[1, 2] = layer.brush
    layer.root[1, 2] = layer.identify
    layer.root[1, 3] = qlayer()

    ## set sizes of layers (arrange the layout)
    set_layout = function() {
        fix_dimension(layer.root,
                      row = list(id = c(0, 2, 3), value = c(prefer_height(meta$main),
                                                  prefer_height(meta$xlabels),
                                                  prefer_height(meta$xlab))),
                      column = list(id = c(1, 0, 3), value = c(prefer_width(meta$ylabels),
                                                     prefer_width(meta$ylab, FALSE),
                                                     10)))
    }
    set_layout()

    ## layout is dynamic (listen to changes in xlab/ylab/xlabels/ylabels...)
    meta$mainChanged$connect(set_layout)
    meta$xlabChanged$connect(set_layout); meta$ylabChanged$connect(set_layout)
    meta$xlabelsChanged$connect(set_layout); meta$ylabelsChanged$connect(set_layout)

    ## finally create the view and set window title
    view = qplotView(scene = scene)
    view$setWindowTitle(paste("Map:", as.character(z$data)))

    ## listeners on the data (which column updates which layer(s))
    d.idx = add_listener(data, function(i, j) {
        idx = which(j == c('.brushed', '.color', '.border'))
        if (length(idx) < 1) {
            compute_coords()
            qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
            layer.main$invalidateIndex(); qupdate(layer.main)
            return()
        } else idx = c(1, 2, 2)[idx]
        switch(idx, qupdate(layer.brush), {
            compute_colors(); qupdate(layer.main)
        })
    })

    if (!is.null(linkto)) {
        id = link_cat(linkto, as.character(z$linkby), data, 'labels')
    }
    ## when layer is destroyed, remove the listener from data
    qconnect(layer.main, 'destroyed', function(x) {
        ## b$colorChanged$disconnect(b.idx)
        remove_listener(data, d.idx)
        if (!is.null(linkto)) {
            remove_link(linkto, id[1]); remove_link(data, id[2])
        }
    })

    ## when b$cursor is changed, update cursor on screen
    b$cursorChanged$connect(function() {
        set_cursor(view, b$cursor)
    })

    ## these layers have the same limits from meta$limits
    sync_limits(meta, layer.main, layer.brush, layer.identify)

    ## simulate brushing
    meta$manual.brush = function(pos) {
        brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
    }

    ## attach meta to the returned value (for post-processing or debugging)
    attr(view, 'meta') = meta
    view
}

Map.meta =
    setRefClass("Map_meta",
                fields = signalingFields(c(

                Common.meta,

                list(group = 'numeric')

                )))



##' Create data for drawing maps
##'
##' This function converts maps data in the \pkg{maps} package to a
##' suitable format for \pkg{cranvas}.
##'
##' The function \code{\link[maps]{map}} is used to convert maps data
##' to a list, then the region names are stored in a mutaframe created
##' by \code{\link{qdata}}; the polygon coordinates are stored in an
##' attribute \code{MapData}. If multiple polygons belong to the same
##' upper-level region, the column \code{labels} will store the
##' upper-level region names.
##' @param database see \code{\link[maps]{map}}
##' @param regions see \code{\link[maps]{map}}
##' @inheritParams qdata
##' @return A mutaframe of region names and labels, with an attribute
##' \code{MapData} containing the coordinates of polygons.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples library(cranvas); map_qdata('state'); map_qdata('county', 'iowa')
map_qdata = function(database, regions = '.', color = NA, border = 'black') {
    df = map(database, regions, plot = FALSE, fill = TRUE)
    ## usually ':' is the separator but sometimes it is ','
    labels =
        if (any(grepl(':', df$names, fixed = TRUE))) {
            sapply(strsplit(df$names, ':', fixed = TRUE), `[`, 1)
        } else if (any(grepl(',', df$names, fixed = TRUE))) {
            sapply(strsplit(df$names, ',', fixed = TRUE), `[`, 1)
        } else df$names
    mf =
        qdata(data.frame(names = df$names, labels = labels, stringsAsFactors = FALSE),
              color = color, border = border)
    attr(mf, 'MapData') = as.data.frame(df[1:2])
    mf
}

##' Calculate coordinates of transformed polygons to make cartograms
##'
##' Based on the given sizes of polygons, this function calculates the
##' transformed coordinates using the \pkg{Rcartogram} package.
##' @param x,y the x and y coordinates of original polygons (polygons
##' are separated by \code{NA}'s)
##' @param size the size vector of polygons (length must be equal to
##' the number of polygons, i.e. the number of \code{NA}'s plus 1)
##' @param nrow,ncol numbers to define a grid for the cartogram
##' algorithm (see references in \pkg{Rcartogram}); this can affect
##' the convergence and speed of the algorithm, so may need to be
##' adjusted for a few times
##' @param ... other arguments passed to
##' \code{\link[Rcartogram]{cartogram}}
##' @return A data frame of two columns \code{x} and \code{y}
##' (coordinates of transformed polygons)
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example inst/examples/cart_polygon-ex.R
cart_polygon = function(x, y, size, nrow = 100, ncol = 100, ...) {
    if (!require('Rcartogram')) {
        message("this function requires the Rcartogram package")
        return(data.frame(x, y))
    }
    if (length(size) != sum(is.na(x)) + 1)
        stop("the length of 'size' vector must be the same as the number of polygons")

    xlim = range(x, na.rm = TRUE); ylim = range(y, na.rm = TRUE)
    dx = c(0, diff(xlim)/1000); dy = c(0, diff(ylim)/1000)  # to construct query rectangle
    grid = matrix(NA, nrow = nrow, ncol = ncol)
    gx = seq(xlim[1], xlim[2], length = ncol); gy = seq(ylim[1], ylim[2], length = nrow)

    ## we use Qt to query the sizes for grid points
    h = qlayer(paintFun = function(layer, painter) {
        qdrawPolygon(painter, x, y)
    }, limits = qrect(xlim, ylim))
    ## generate the population density matrix (time consuming)
    message('Generating the population density grid...')
    pb = txtProgressBar(min = 0, max = nrow, style = 3)
    for (i in seq_len(nrow)) {
        for (j in seq_len(ncol)) {
            hit = h$locate(qrect(gx[j] + dx, gy[i] + dy))
            if (length(hit)) grid[i, j] = size[hit[1] + 1]
        }
        setTxtProgressBar(pb, i)
    }
    close(pb)
    f = min(grid, na.rm = TRUE)
    grid[is.na(grid)] = f  # fill NA's with mean; add margin with mean too later
    grid = addBoundary(grid, land.mean = f)
    extra = attr(grid, 'extra')  # extra rows/cols added
    message('Calculating cartogram coordinates...')
    res = cartogram(grid, ...)
    pred =
        predict(res, (x - xlim[1]) / (diff(xlim)) * (ncol - 1) + 1 + extra[1],
                (y - ylim[1]) / (diff(ylim)) * (nrow - 1) + 1 + extra[2])
    data.frame(x = (pred$x - extra[1] - 1) / (ncol - 1) * diff(xlim) + xlim[1],
               y = (pred$y - extra[2] - 1) / (nrow - 1) * diff(ylim) + ylim[1])
}
