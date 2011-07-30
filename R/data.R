

##' Give a column-wise scalar summary of the data frame or matrix
##'
##' For numeric columns, the function specified by \code{fun} will be applied
##' (\code{median()} by default); for other types of columns, the mode will be
##' caculated (i.e. the categories that have the maximum frequencies) and the
##' returned value is a single string with the category labels separated by
##' \code{', '}.
##' @param x the data frame or matrix to be summarized
##' @param fun the function to be applied to numeric columns
##' @param ... other arguments to be passed to \code{fun}
##' @return a vector corresponding to columns of \code{x}
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' summary_one(iris)
##' summary_one(iris[1:70, ])
##' summary_one(mtcars)
summary_one = function(x, fun = median, ...) {
    ## it is not easy to use this function in qdata(); current implementation
    ## is ugly; we need mutalist; but how many people really care about the
    ## summarizing function other than median?
    x = as.data.frame(x)
    mode_label = function(x) {
        tb = table(x)
        paste(names(which(tb == max(tb))), collapse = ", ")
    }
    sapply(x, function(xx) {
        ifelse(is.numeric(xx), fun(xx, ...), mode_label(xx))
    })
}


## remove (nearly) constant columns
.rm.cons.col = function(data) {
    const.col = sapply(data, function(x) {
        x = na.omit(x)
        x = as.numeric(x)
        length(x) == 0 || diff(range(x)) < 1e-6
    })
    if (any(const.col)) {
        data = data[, !const.col, drop = FALSE]
        warning("removed constant column(s) ",
                paste(names(data)[const.col], collapse = ","))
    }
    data
}
