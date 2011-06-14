##' Select a subset of data through a GUI to be brushed.
##'
##' We use a GUI based on \pkg{gWidgetsQt} to subset the data based on
##' the variable given. Specifically, we choose certain values of the
##' variable (using the mouse or keyboard) and all the observations
##' which have the same values on this variable will be brushed. This
##' selector can link to any plots based on a mutaframe created by
##' \code{\link{qdata}}.
##' @param data a mutaframe (with a column \code{.brushed})
##' @param vars character or integer: the variable to be displayed in
##' the data selector (if not specified, the first non-numeric
##' variable will be used; if all columns are numeric, the first
##' column will be used)
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples
##' ## old iris as the toy example
##' qiris = qdata(iris)
##' qparallel(data = qiris)
##' record_selector(qiris)
##'
##' ## NRC rankings
##' data(nrcstat)
##' qnrc = qdata(nrcstat)
##' qparallel(vars = 10:13, data = qnrc, main = 'Overview of Rankings', horizontal=FALSE)
##' record_selector(qnrc, 'Institution.Name')
##' qparallel(vars = 14:19, data = qnrc, main = 'Research, Student Support, Diversity')
##' qparallel(vars = 20:26, data = qnrc, main = 'Publication, Award, Time to Degree')
##'
record_selector = function(data, vars) {
    if (missing(vars)) {
        vars = names(data)[!(sapply(as.data.frame(data), class) %in% c("numeric",
            "integer"))][1]
        if (is.na(vars))
            vars = names(data)[1]
    }
    ## vars should be of length 1
    x = data[, vars[1]]
    xx = as.data.frame(data[!duplicated(x), vars[1], drop = FALSE])
    if (!require('gWidgetsQt')) {
        warning("You need to install.packages('gWidgetsQt', repos = 'http://r-forge.r-project.org') after installing 'qtutils' from https://github.com/ggobi/qtutils")
        return()
    }
    options(guiToolkit = 'Qt')
    gg = ggroup(horizontal = FALSE, container = gwindow("Record Selector"))
    gtbl = gtable(xx, multiple = TRUE, container = gg, expand = TRUE)
    addHandlerClicked(gtbl, handler = function(h, ...) {
        data$.brushed = (x %in% svalue(h$obj))
    })
    gtxt = gedit(container = gg)
    addHandlerChanged(gtxt, handler = function(h, ...) {
        idx = if (svalue(h$obj) != "") {
            grep(svalue(h$obj), as.character(x), ignore.case = TRUE)
        } else integer(0)
        svalue(gtbl) = x[idx]
        .brushed = logical(length(x))
        .brushed[idx] = TRUE
        selected(data) = .brushed
    })
    invisible(NULL)
}
