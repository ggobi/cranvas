##' Data Imputation
##'
##' Impute data by various methods.
##' @title Data Imputation
##' @param x the numeric data matrix
##' @param method imputation method; one of the following:
##' \describe{
##'   \item{below.min}{replace missing values by a value 20\% below the mininum}
##' }
##' @return the imputed data
##' @author Yihui Xie <\url{http://yihui.name}>
na.impute = function(x, method = 'below.min') {
    apply(x, 2, function(xx) {
        if (any(is.na(xx))) {
            xx[is.na(xx)] = switch(method,
              'below.min' = min(xx, na.rm = TRUE) - 0.2 * diff(range(xx, na.rm = TRUE)))
        }
        xx
    })
}


##' Give a column-wise scalar summary of the data frame or matrix.
##'
##' For numeric columns, the function specified by \code{fun} will be applied
##' (\code{median()} by default); for other types of columns, the mode will be
##' caculated (i.e. the categories that have the maximum frequencies) and the
##' returned value is a single string with the category labels separated by
##' \code{', '}.
##' @title Give a Column-wise Scalar Summary of the Data Frame or Matrix
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
    ## it is not easy to use this function in qmutaframe(); current implementation
    ## is ugly; we need mutalist; but how many people really care about the
    ## summarizing function other than median?
    x = as.data.frame(x)
    mode_label = function(x) {
        tb = table(x)
        paste(names(which(tb == max(tb))), collapse = ', ')
    }
    sapply(x, function(xx){
        ifelse(is.numeric(xx), fun(xx, ...), mode_label(xx))
    })
}
