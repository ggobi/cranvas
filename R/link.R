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
##' \code{\link{remove_link}} (see examples below).
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
##' @seealso \code{\link{qdata}}
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
    id = c(add_listener(mf1, function(i, j) {
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
    l1 = attr(mf1, 'Link'); l1$linkid = c(l1$linkid, id[1])
    if (link2) {
        l2 = attr(mf2, 'Link'); l2$linkid = c(l2$linkid, id[2])
    }
    id
}

##' k-Nearest neighbor linking
##'
##' When a number of elements in a data are brushed, their k-nearest
##' neighbors (based on a certain distance measure) are brushed as
##' well.
##'
##' A center point for the variables based on the selected rows is
##' calculated in the first dataset, then the k nearest rows in the
##' second dataset (if not provided, it will be the same as the first
##' dataset) to this center are selected. Only the Euclidean distance
##' has been implemented at the moment.
##' @inheritParams link_cat
##' @param var1 the variable names or column indices of the first
##' mutaframe to be used to calculate distances
##' @param var2 (optional) variable names or column indices of the
##' second mutaframe (by default the same as \code{var1})
##' @param k the number of nearest neighbors to select
##' @return Similar to categorical linking (\code{\link{link_cat}}),
##' this function also links two mutaframes together (or one mutaframe
##' to itself), and id's of listeners are returned.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @example inst/examples/link_knn-ex.R
link_knn = function(mf1, var1 = NULL, mf2 = NULL, var2 = var1, k = 10, ...) {
    link2 = !is.null(mf2)
    if (!check_data(mf1, FALSE) || (link2 && !check_data(mf2, FALSE)))
        stop('the mutaframes must be created from qdata()')
    if (is.null(var1) || (link2 && is.null(var2)))
        stop("must specify linking variables")
    if (link2 && (length(var1) != length(var2)))
        stop("'var1' and 'var2' must have the same length")
    change1 = change2 = FALSE
    id = c(add_listener(mf1, function(i, j) {
        if (j != '.brushed' || ifelse(link2, change1, change2)) return()
        change2 <<- TRUE
        df1 = as.matrix(as.data.frame(mf1[, var1, drop = FALSE]))
        if (any(idx <- selected(mf1))) {
            sf1 = df1[idx, , drop = FALSE]
            cf1 = colMeans(sf1)         # center
            if (link2) {
                df2 = as.matrix(as.data.frame(mf2[, var2, drop = FALSE]))
                selected(mf2) = rank(apply(sweep(df2, 2, cf1)^2, 1, sum)) <= k
            } else selected(mf1) = rank(apply(sweep(df1, 2, cf1)^2, 1, sum)) <= k
        } else if (link2) selected(mf2) = FALSE
        change2 <<- FALSE
    }), if (link2) add_listener(mf2, function(i, j) {
        if (j != '.brushed' || change2) return()
        change1 <<- TRUE
        df2 = as.matrix(as.data.frame(mf2[, var2, drop = FALSE]))
        if (any(idx <- selected(mf2))) {
            sf2 = df2[idx, , drop = FALSE]
            cf2 = colMeans(sf2)
            df1 = as.matrix(as.data.frame(mf1[, var1, drop = FALSE]))
            selected(mf1) = rank(apply(sweep(df1, 2, cf2)^2, 1, sum)) <= k
        } else if (link2) selected(mf2) = FALSE
        change1 <<- FALSE
    }))
    l1 = attr(mf1, 'Link'); l1$linkid = c(l1$linkid, id[1])
    if (link2) {
        l2 = attr(mf2, 'Link'); l2$linkid = c(l2$linkid, id[2])
    }
    id
}

##' Remove the linking in mutaframes
##'
##' This function removes the linking (listeners) in mutaframes.
##'
##' @inheritParams qbar
##' @param id a character vector of id's of linking listeners (can be
##' obtained as the returned value of \code{\link{link_cat}} and
##' \code{\link{link_knn}}); by default it is a vector of all id's for
##' linking
##' @return the listeners are removed
##' @author Yihui Xie <\url{http://yihui.name}>
##' @seealso \code{\link{link}}
##' @export
##' @examples ## see ?link_cat
remove_link = function(data = last_data(), id = link_id(data)) {
    l = attr(data, 'Link')
    for (j in id) {
        remove_listener(data, j)
        l$linkid = setdiff(l$linkid, j)
    }
}

##' Get the id's of listeners on linking
##'
##' This function returns the id's of listeners attached on a data for
##' linking, and these id's can be used to remove the linking.
##' @inheritParams qbar
##' @return A character vector of id's.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples ## see ?link_cat
link_id = function(data = last_data()) {
    attr(data, 'Link')$linkid
}
