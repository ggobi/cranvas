##' Link two mutaframes by a common categorical variable.
##'
##' This function links two mutaframes together by a common
##' categorical variable; this linking variable can be specified with
##' \code{\link{link_var}}.
##'
##' @param mf1 the first mutaframe
##' @param mf2 the second mutaframe
##' @return the two mutaframes will be linked together by their
##' linking variables (listeners added)
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example cranvas/inst/examples/link-ex.R
link = function(mf1, mf2) {
    if (!is.mutaframe(mf1) || !is.mutaframe(mf2) || is.null(link1 <- link_var(mf1)) || is.null(link2 <- link_var(mf2))) {
        stop('both mf1 and mf2 must be mutaframes and have a linking variable specified respectively',
             '\n  see ?link_var')
    } else {
        if (sum('.brushed' == c(colnames(mf1), colnames(mf2))) != 2) {
            stop("both mf1 and mf2 must have a column '.brushed' respectively",
                 '\n  see ?qmutaframe')
        } else {
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
