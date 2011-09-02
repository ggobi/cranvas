##' Graphical parameters in cranvas
##'
##' This function can set or query the graphical parameters in the
##' \code{cranvas} package.
##'
##' @param ... options of the form \code{tag = value}
##' @return the current list of parameters or set new options
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link[graphics]{par}}
##' @export
##' @examples
##' op = qpar()
##' qpar(mar = 0.05)  # the degree to extend the plot margin (inner margin)
##' qpar(op)  # restore
##'
qpar = function(...) {
    p = list(...)
    .opts = .cranvasEnv$.opts
    if (length(p)) {
        if (is.null(names(p)) && !is.list(p[[1]])) {
            p = unlist(p)
            if (length(p) == 1)
                .opts[[p]]
            else .opts[p]
        }
        else {
            omf = .opts
            if (is.list(p[[1]]))
                p = p[[1]]
            if (length(p) > 0) {
                .opts[names(p)] = p
                .cranvasEnv$.opts = .opts
            }
            invisible(omf)
        }
    }
    else {
        .opts
    }
}
