##' Link mutaframes by a common categorical variable.
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
##' @seealso \code{\link{qdata}}, \code{\link{link_var}},
##' \code{\link{link_type}}, \code{\link{remove_link}}
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
    id = vector(mode = 'list', length = n)
    cb = combn(n, 2)
    for (k in 1:ncol(cb)) {
        ## chain them in a circle
        k1 = cb[1, k]
        k2 = cb[2, k]
        mf1 = s[[k1]]
        mf2 = s[[k2]]
        if (identical(mf1, mf2)) {
            warning(sprintf('mutaframe %d and %d are identical and will not be linked; ',
                            k1, k2),
                    sprintf('please set link_type(%s) <- "self"',
                            as.character(match.call()[-1])[k1]))
            next
        }
        link1 = link_var(mf1)
        link2 = link_var(mf2)
        id[[k1]] = append(id[[k1]], add_listener(mf1, function(i, j) {
            if (focused(mf1)) {
                ## mf1 changed --> query link1 --> match link2 --> change mf2$.brushed
                ulink1 = unique(mf1[, link1][mf1$.brushed])
                ## update mf2$.brushed according to mf1's selected categories
                mf2$.brushed = mf2[, link2] %in% ulink1
            }
        }))
        id[[k2]] = append(id[[k2]], add_listener(mf2, function(i, j) {
            if (focused(mf2)) {
                ulink2 = unique(mf2[, link2][mf2$.brushed])
                mf1$.brushed = mf1[, link1] %in% ulink2
            }
        }))
    }
    id
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
##' link_var(mf) = NULL  # disable linking
link_var = function(data) {
    attr(data, "Link")[["linkvar"]]
}

##' @rdname link_var
##' @usage link_var(data) <- value
##' @param value the name of the linking variable (or \code{NULL} to
##' disable linking)
##' @return set the linking variable
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

##' Remove the linking between mutaframes.
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

##' Update a mutaframe by the linking on itself through a categorical variable.
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
##' self_link(mf)
##' selected(mf)
self_link = function(data) {
    if ((!('self' %in% link_type(data))) || is.null(v <- link_var(data))) return()
    selected(data) = data[, v] %in% unique(data[selected(data), v])
}
