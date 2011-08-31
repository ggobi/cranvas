##' Draw a missing value plot
##'
##' A missing value plot shows the counts or proportions of missing
##' values in each variable. It is essentially a stacked bar plot,
##' i.e. a bar plot of variables split by the logical vectors of
##' missingness of observations.
##'
##' As usual, common interactions are defined in
##' \code{\link{common_key_press}}. Brushing on a missing value plot
##' has a slightly different meaning with brushing other types of
##' plots: if a rectangle is brushed in a missing value plot, all rows
##' in the orginal data in which the current variable is brushed
##' (i.e. either missing or non-missing) are brushed; on the other
##' hand, the brushed rows in the original data will also be reflected
##' in the missing value plot.
##'
##' This plot is built upon the bar plot \code{\link{qbar}}.
##' @param vars variables to show in the plot: a character vector of
##' variable names, or a numeric vector of column indices, or a
##' two-sided formula like \code{~ x1 + x2 + x3} (without the
##' left-hand side)
##' @inheritParams qbar
##' @param ... arguments passed to the default method and further to
##' \code{\link{qbar}}
##' @return A missing value plot
##' @author Heike Hofmann and Yihui Xie
##' @export
##' @family plots
##' @example inst/examples/qmval-ex.R
qmval = function(vars, data = last_data(), ...) {
    UseMethod('qmval')
}
##' @method qmval default
##' @rdname qmval
##' @export
qmval.default =
    function(vars, data = last_data(), horizontal = TRUE, standardize = TRUE, ...) {
        shadow = attr(data, 'Shadow')
        if (is.null(shadow)) stop('there are no missing values in the data!')
        ## reshape the shadow matrix to a new qdata()
        d =
            data.frame(variable = rep(vars, each = nrow(data)),
                       missing = factor(as.vector(shadow[, vars]), c(TRUE, FALSE)))
        nd = qdata(d, color = missing, copy = FALSE)
        ## link nd to data (code borrowed from link.R)
        change1 = change2 = FALSE
        add_listener(nd, function(i, j) {
            if (change1) return()
            change2 <<- TRUE
            if (j == '.brushed') {
                selected(data) = apply(matrix(selected(nd), ncol = length(vars)), 1, any)
            } else if (j == '.visible') {
                visible(data) = apply(matrix(visible(nd), ncol = length(vars)), 1, all)
            }
            change2 <<- FALSE
        })
        add_listener(data, function(i, j) {
            if (change2) return()
            change1 <<- TRUE
            if (j == '.brushed') {
                selected(nd) = rep(selected(data), length(vars))
            } else if (j == '.visible') {
                visible(nd) = rep(visible(data), length(vars))
            }
            change1 <<- FALSE
        })
        qbar(variable, data = nd, horizontal = horizontal, standardize = standardize, ...)
    }
##' @method qmval numeric
##' @rdname qmval
##' @export
qmval.numeric = function(vars, data = last_data(), ...) {
    qmval(names(data)[vars], data, ...)
}
##' @method qmval formula
##' @rdname qmval
##' @export
qmval.formula = function(vars, data = last_data(), ...) {
    if (length(vars) != 2) stop("'vars' must be a one-sided formula!")
    v = all.vars(vars)
    if (identical(v, '.')) v = grep('^[^.]', names(data), value = TRUE)
    qmval(v, data, ...)
}
