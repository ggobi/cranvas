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
##' (e.g. rectangles); possible values are similar to \code{color} but
##' the default value \code{NULL} has a special meaning: the fill
##' colors will be the same as the \code{color} argument but with more
##' brightness (e.g. pure black will become gray; see
##' \code{\link{lighter}})
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
qdata = function(data, color = "black", fill = NULL, size = 1, brushed = FALSE, visible = TRUE) {
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
    l = list()  # record scales in a list
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
            l[[i]] = list(label = deparse(z[[i]]), value = v)
        } else {
            mf[[sprintf('.%s', i)]] = if (i != 'fill') {
                switch(i, color = color, size = size)
            } else {
                if (is.null(fill)) {
                    lighter(mf$.color)  # default is use lighter .color
                } else fill
            }
        }
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
            if (is.null(j)) return()
            if (!(j %in% row_attrs))
                attr(mf, 'Shadow') = is.na(as.data.frame(mf[, !(names(mf) %in% row_attrs)]))
        })  # shadow matrix will change when data is changed
    }

    attr(mf, 'Scales') = l  # scales information to be used in legend
    attr(mf, 'Generator') = 'd38bbe46dae5fa45758f3609f5dc1a0a'  # a token for internal use
    .cranvasEnv$.last.data = mf  # make a copy to .last.data
    mf
}

.cranvasEnv$.last.data = NULL

##' Get the last used data object
##'
##' Since interactive graphics often involves with linking based on
##' the same data object, this function provides an access to the last
##' used data object, which is often the default value for the
##' argument \code{data} in many plotting functions in this package.
##' @return The last data object created by \code{\link{qdata}}.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples library(cranvas)
##' data(nrcstat)
##' qnrc = qdata(nrcstat, color = Regional.Code)
##' qbar(Regional.Code, data = last_data())
##' ## or simply ignore the data argument
##' qbar(Regional.Code)
last_data = function() {
    if (is.null(.cranvasEnv$.last.data))
        stop('No data object was created by qdata() yet') else .cranvasEnv$.last.data
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
##' @examples df = qdata(mtcars)
##'
##' selected(df)
##'
##' selected(df) = rep(c(TRUE, FALSE), c(10, 22))  # brush the first 10 obs
##' selected(df)
##'
##' selected(df) = 15L  # brush the 15th row
##' selected(df)
##'
##' selected(df) = 'Honda Civic'  # brush by row names
selected = function(data) {
    if ('.brushed' %in% names(data))
        data$.brushed else logical(nrow(data))
}
##' @rdname selected
##' @usage selected(data) <- value
##' @param value a logical vector of the length \code{nrow(data)}, or
##' a vector of integers which will be used to create a logical vector
##' with \code{TRUE} corresponding to these indicies, or a character
##' vector of row names to brush the corresponding rows
##' @export "selected<-"
`selected<-` = function(data, value) {
    ## if value is numeric indices or names, convert it to a logical vector
    if (is.numeric(value) || is.character(value)) {
        tmp = logical(nrow(data))
        names(tmp) = rownames(data)
        tmp[value] = TRUE
        value = unname(tmp)
    }
    data$.brushed = value
    data
}

##' Set or query the linking variable in a mutaframe
##'
##' @param data the mutaframe (typically created by
##' \code{\link{qdata}}), with an attribute \code{Link}
##' @return \code{\link{link_var}} returns the name of the linking
##' variable
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @seealso \code{\link{qdata}}, \code{\link{link}}
##' @examples
##' mf = qdata(head(iris))
##' link_var(mf)  # NULL
##' link_var(mf) = 'Species'  # linking by 'Species'
##' link_var(mf)
##' link_var(mf) = NULL  # disable linking
link_var = function(data) {
    attr(data, "Link")[["linkvar"]]
}

##' @rdname link_var
##' @usage link_var(data) <- value
##' @param value the name of the linking variable (or \code{NULL} to
##' disable linking); the variable must be a factor (i.e. categorical
##' variable)
##' @export "link_var<-"
`link_var<-` = function(data, value) {
    if (!is.null(value)) {
        if (!(value %in% colnames(data)))
            stop(value, " is not in the column names of data")
        if (!is.factor(data[, value]))
            stop('currently only support linking through categorical variables (factors)')
    }
    attr(data, "Link")[["linkvar"]] = value
    data
}

##' Set or query the type of linking
##'
##' Types of linking include hot, cold and self linking. Hot linking
##' means other plots get updated immediately after the current plot
##' is brushed; cold linking will not update other plots until they
##' are on focus; self linking means all the elements in the same
##' category as the current brushed element(s) will be brushed as
##' well.
##'
##' @param data the mutaframe (typically created by
##' \code{\link{qdata}}), with an attribute \code{Link}
##' @return \code{\link{link_type}} returns the type of linking
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples
##' mf = qdata(iris)
##' link_type(mf)
##' link_type(mf) = 'self'
##' link_type(mf) = 'cold'
link_type = function(data) {
    attr(data, 'Link')$type
}
##' @rdname link_type
##' @usage link_type(data) <- value
##' @param value the type of linking (possible values are \code{hot},
##' \code{cold} and \code{self})
##' @export "link_type<-"
`link_type<-` = function(data, value) {
    attr(data, 'Link')$type = value
    data
}

##' Check if a data object was created by qdata()
##'
##' Data objects created by \code{\link{qdata}} has a special
##' token. If an object was not created in that way, it will be
##' converted by \code{\link{qdata}}.
##'
##' This function is designed for developers to check the validity of
##' data objects.
##' @param data a data object
##' @return If \code{data} was created by \code{\link{qdata}}, it will
##' be returned untouched, otherwise \code{qdata(data)} is returned.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples check_data(cbind(x = 1:5, y = 6:10))
check_data = function(data) {
    if (identical(attr(data, 'Generator'), 'd38bbe46dae5fa45758f3609f5dc1a0a'))
        data else {
            message(paste(strwrap('Automatically converting to a mutaframe... Interaction will work based on this data, but will not link to any other plots. For linking to work, use qdata() to create data objects.'), collapse = '\n'))
            qdata(data)
        }
}
