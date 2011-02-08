##' Link mutaframes by a common categorical variable.
##'
##' This function links several mutaframes together by a common
##' categorical variable; this linking variable can be specified with
##' \code{\link{link_var}}.
##'
##' @param ... the mutaframes (at least two mutaframes); the
##' mutaframes are typically created by \code{\link{qmutaframe}}
##' @return the mutaframes will be linked together by their
##' linking variables (listeners added)
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example cranvas/inst/examples/link-ex.R
link = function(...) {
    s = list(...)
    if ((n <- length(s)) < 2)
        stop('the number of mutaframes to be linked must be greater than one')
    for (k in 1:n) {
        if (!is.mutaframe(s[[k]]))
            stop('argument ', k, ' is not a mutaframe')
        if (is.null(link_var(s[[k]])))
            stop('mutaframe ', k, ' must have a linking variable; see ?link_var')
        if (!('.brushed' %in% colnames(s[[k]])))
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
##' \code{\link{qmutaframe}}), with an attribute \code{.linking}
##' @return the name of the linking variable
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export link_var
##' @export "link_var<-"
##' @examples
##' mf = qmutaframe(head(iris))
##' link_var(mf)  # NULL
##' link_var(mf) = 'Species'  # linking by 'Species'
##' link_var(mf)
##'
link_var = function(data) {
    attr(data, '.linking')[['.linkvar']]
}

##' @rdname link_var
##' @usage link_var(data) <- value
##' @param value the name of the linking variable
##' @return set the linking variable
`link_var<-` = function(data, value) {
    if (!(value %in% colnames(data)))
        stop(value, ' is not in the column names of data')
    attr(data, '.linking')[['.linkvar']] = value
    data
}
