##' Select a subset of data through a GUI to be brushed.
##'
##' We use a \pkg{gWidgets}-based GUI to subset the data based on the
##' variable given. Specifically, we choose certain values of the
##' variable (using the mouse or keyboard) and all the observations
##' which have the same values on this variable will be brushed. This
##' selector can link to any plots based on a mutaframe.
##' @param data a mutaframe (ideally with a column \code{.brushed})
##' @param vars character or integer: the variable to be displayed in
##' the data selector (if not specified, the first non-numeric
##' variable will be used; if all columns are numeric, the first
##' column will be used)
##' @param gui.type the type of the \pkg{gWidgets} GUI: \code{'RGtk2'}
##' is based on the package \pkg{gWidgetsRGtk2}, and \code{'Qt'} is
##' based on \pkg{gWidgetsQt}
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' if (require('gWidgetsRGtk2')) {
##' ## old iris as the toy example
##' qiris = qdata(iris)
##' qparallel(qiris)
##' record_selector(qiris)
##'
##' ## NRC rankings
##' data(nrcstat)
##' qnrc = qdata(nrcstat)
##' qparallel(qnrc, vars = 10:13, main = 'Overview of Rankings', horizontal=FALSE)
##' record_selector(qnrc, 'Institution.Name')
##' qparallel(qnrc, vars = 14:19, main = 'Research, Student Support, Diversity')
##' qparallel(qnrc, vars = 20:26, main = 'Publication, Award, Time to Degree')
##' }
record_selector = function(data, vars, gui.type = c("RGtk2", "Qt")) {
    if (missing(vars)) {
        vars = names(data)[!(sapply(as.data.frame(data), class) %in% c("numeric", 
            "integer"))][1]
        if (is.na(vars)) 
            vars = names(data)[1]
    }
    ## vars should be of length 1
    x = data[, vars[1]]
    xx = as.data.frame(data[!duplicated(x), vars[1], drop = FALSE])
    gui.type = match.arg(gui.type)
    pkg = paste("gWidgets", gui.type, sep = "")
    if (!require(pkg, character.only = TRUE)) {
        warning("Please first install.packages('", pkg, "').")
        return()
    }
    options(guiToolkit = gui.type)
    gg = ggroup(horizontal = FALSE, container = gwindow("Data Selector"))
    gtbl = gtable(xx, multiple = TRUE, container = gg, expand = TRUE)
    addHandlerClicked(gtbl, handler = function(h, ...) {
        data$.brushed = (x %in% svalue(h$obj))
    })
    gtxt = gedit(container = gg)
    addHandlerChanged(gtxt, handler = function(h, ...) {
        idx = if (svalue(h$obj) != "") {
            grep(svalue(h$obj), as.character(x), ignore.case = TRUE)
        }
        else integer(0)
        svalue(gtbl) = x[idx]
        .brushed = logical(length(x))
        .brushed[idx] = TRUE
        data$.brushed = .brushed
    })
    invisible(NULL)
} 
