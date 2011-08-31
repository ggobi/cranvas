##' Categorical linking
##'
##' This function links two mutaframes together (or one mutaframe to
##' itself) by a common categorical variable so that whenever one
##' element (or multiple elements) in a category (or multiple
##' categories) is brushed, all elements in this (these) categories
##' will be brushed.
##'
##' Categorical linking is achieved by a series of logical operations:
##' first, look for which rows are brushed in the first mutaframe, and
##' find out the values of its linking variable as well as the
##' categories they belong to, then look for which elements of the
##' linking variable in the second mutaframe (possibly the same
##' mutaframe) are in these categories, and brush these elements
##' (corresponding to rows).
##'
##' The implementation is through listeners on mutaframes from the
##' \pkg{plumbr} package. It may be important keep track of the id's
##' of listeners to avoid unnecessary burden of updating data objects
##' in a linking chain. Listeners can be detached from mutaframes by
##' \code{\link[plumbr]{remove_listener}} (see examples below).
##' @param mf1 the first mutaframe
##' @param var1 the name of the linking variable of \code{mf1}
##' @param mf2 (optional) the second mutaframe; default \code{NULL}
##' means \code{mf1} will be linked to itself
##' @param var2 (optional) the name of the linking variable of \code{mf2}
##' @return The mutaframes will be linked together by their linking
##' variables (listeners are added to mutaframes), and the id's of the
##' listeners attached on each mutaframe will be returned as a vector
##' (first element for the first mutaframe; second element for the
##' second one).
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{qdata}}, \code{\link{link_var}},
##' \code{\link{link_type}}
##' @export
##' @example inst/examples/link_cat-ex.R
link_cat = function(mf1, var1, mf2 = NULL, var2 = NULL) {
    link2 = !is.null(mf2)
    if (!check_data(mf1, FALSE) || (link2 && !check_data(mf2, FALSE)))
        stop('the mutaframes must be created from qdata()')
    if (is.null(var1) || (link2 && is.null(var2)))
        stop("must specify linking variables")
    ## is a mutaframe changed? a token to control the listener and avoid infinite recursion
    change1 = change2 = FALSE
    c(add_listener(mf1, function(i, j) {
        if (j != '.brushed' || ifelse(link2, change1, change2)) return()
        change2 <<- TRUE
        ## mf1 changed --> query var1 --> match var2 --> change mf2$.brushed
        ## update mf2$.brushed according to mf1's selected categories
        ulink = unique(mf1[, var1][mf1$.brushed])
        if (link2) {
            mf2$.brushed = mf2[, var2] %in% ulink
        } else mf1$.brushed = mf1[, var1] %in% ulink
        change2 <<- FALSE
    }), if (link2) add_listener(mf2, function(i, j) {
        if (j != '.brushed' || change2) return()
        change1 <<- TRUE
        mf1$.brushed = mf1[, var1] %in% unique(mf2[, var2][mf2$.brushed])
        change1 <<- FALSE
    }))
}

##' k-Nearest neighbor linking
##'
##' When a number of elements in a data are brushed, their k-nearest
##' neighbors (based on a certain distance measure) are brushed as
##' well.
##' @inheritParams link_cat
##' @param var1 the variable names or column indices of the first
##' mutaframe to be used to calculate distances
##' @param var2 (optional) variable names or column indices of the
##' second mutaframe
##' @return Similar to categorical linking (\code{\link{link_cat}}),
##' this function also links two mutaframes together (or one mutaframe
##' to itself), and id's of listeners are returned.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples ## TODO
link_knn = function(mf1, var1, mf2 = NULL, var2 = NULL, ...) {

}

##' Remove the linking between mutaframes
##'
##' This is the reverse operation to \code{\link{link}}, i.e., this
##' function removes the linking (listeners) between mutaframes.
##'
##' @param ... the mutaframes from which the linking will be removed
##' @param id a list of the id's of listeners (can be obtained as the
##' returned value of \code{\link{link}})
##' @return the listeners are removed
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{link}}
##' @export
##' @examples ## see ?link
remove_link = function(..., id) {
    s = list(...)
    if (!is.list(id) || (n <- length(s)) != length(id))
        stop('id must be a list of the same length as the number of mutaframes passed in')
    for (i in 1:n) {
        for (j in id[[i]]) remove_listener(s[[i]], j)
    }
}

self_link = function(data) {
    if ((!('self' %in% link_type(data))) || is.null(v <- link_var(data))) return()
    selected(data) = data[, v] %in% unique(data[selected(data), v])
}
