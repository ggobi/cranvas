##' Link mutaframes by a common categorical variable.
##'
##' This function links several mutaframes together by a common
##' categorical variable; this linking variable can be specified with
##' \code{\link{link_var}}.
##'
##' @param ... the mutaframes (at least two mutaframes); the
##' mutaframes are typically created by \code{\link{qdata}}
##' @return the mutaframes will be linked together by their
##' linking variables (listeners added)
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{qdata}}, \code{\link{link_var}}
##' @export
##' @example cranvas/inst/examples/link-ex.R
link = function(...) {
    s = list(...)
    if ((n <- length(s)) < 2)
        stop("the number of mutaframes to be linked must be greater than one")
    for (k in 1:n) {
        if (!is.mutaframe(s[[k]]))
            stop("argument ", k, " is not a mutaframe")
        if (is.null(link_var(s[[k]])))
            stop("mutaframe ", k, " must have a linking variable; see ?link_var")
        if (!(".brushed" %in% colnames(s[[k]])))
            stop("mutaframe ", k, " must have a column '.brushed'")
    }
    for (k in 1:(n - 1)) {
        ## chain them in a circle
        mf1 = s[[k]]
        mf2 = s[[ifelse(k + 1 <= n, k + 1, 1)]]
        link1 = link_var(mf1)
        link2 = link_var(mf2)
        add_listener(mf1, function(i, j) {
            if (focused(mf1)) {
                ## mf1 changed --> query link1 --> match link2 --> change mf2$.brushed
                ulink1 = unique(mf1[, link1][mf1$.brushed])
                ## update mf2$.brushed according to mf1's selected categories
                mf2$.brushed = mf2[, link2] %in% ulink1
            }
        })
        add_listener(mf2, function(i, j) {
            if (focused(mf2)) {
                ulink2 = unique(mf2[, link2][mf2$.brushed])
                mf1$.brushed = mf1[, link1] %in% ulink2
            }
        })
    }
}

##' Set or query the linking variable in a mutaframe.
##'
##' @param data the mutaframe (typically created by
##' \code{\link{qdata}}), with an attribute \code{Link}
##' @return the name of the linking variable
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export link_var
##' @export "link_var<-"
##' @seealso \code{\link{qdata}}, \code{\link{link}}
##' @examples
##' mf = qdata(head(iris))
##' link_var(mf)  # NULL
##' link_var(mf) = 'Species'  # linking by 'Species'
##' link_var(mf)
##'
link_var = function(data) {
    attr(data, "Link")[["linkvar"]]
}

##' @rdname link_var
##' @usage link_var(data) <- value
##' @param value the name of the linking variable
##' @return set the linking variable
`link_var<-` = function(data, value) {
    if (!(value %in% colnames(data)))
        stop(value, " is not in the column names of data")
    attr(data, "Link")[["linkvar"]] = value
    data
}

##' Set or query the type of linking.
##' Types of linking include hot, cold and self linking. Hot linking
##' means other plots get updated immediately after the current plot
##' is brushed; cold linking will not update other plots until they
##' are on focus; self linking means all the elements in the same
##' category as the current brushed element(s) will be brushed as
##' well.
##'
##' @param data the mutaframe (typically created by
##' \code{\link{qdata}}), with an attribute \code{Link}
##' @return the type of linking
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
##' @return set the linking type
##' @export "link_type<-"
`link_type<-` = function(data, value) {
    attr(data, 'Link')$type = value
    data
}
