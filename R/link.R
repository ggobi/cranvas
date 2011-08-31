##' Link mutaframes by a common categorical variable
##'
##' This function links several mutaframes together by a common
##' categorical variable; this linking variable can be specified with
##' \code{\link{link_var}}.
##'
##' @param ... the mutaframes (at least two different mutaframes); the
##' mutaframes are typically created by \code{\link{qdata}}
##' @return the mutaframes will be linked together by their linking
##' variables (listeners added), and the id's of the listeners
##' attached on each mutaframe will be returned as a list.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @section Warning: The mutaframes must be different, otherwise the
##' linking will end up with infinite recursion. For example,
##' \code{link(data1, data1)} is not allowed.
##' @seealso \code{\link{qdata}}, \code{\link{link_var}},
##' \code{\link{link_type}}, \code{\link{remove_link}}
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

##' Update a mutaframe by the linking on itself through a categorical
##' variable
##'
##' If the linking type is \code{self} on a mutaframe, and the linking
##' variable has been specified, this function will brush all the rows
##' which are in the same category or categories of the current
##' brushed elements.
##' @param data a mutaframe created by \code{\link{qdata}}
##' @return the \code{.brushed} column in the mutaframe is updated
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{link_var}}, \code{\link{link_type}}
##' @export
##' @examples mf = qdata(iris)
##' selected(mf)[1] = TRUE  # brush the 1st row
##' selected(mf)
##'
##' link_var(mf) = 'Species'
##' link_type(mf) = 'self'
##' self_link(mf)  # all the first 50 rows are brushed
##' selected(mf)
self_link = function(data) {
    if ((!('self' %in% link_type(data))) || is.null(v <- link_var(data))) return()
    selected(data) = data[, v] %in% unique(data[selected(data), v])
}
