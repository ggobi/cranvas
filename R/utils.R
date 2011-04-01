##' Extend the range of data by an amount \code{qpar('mar')}.
##'
##' This is useful to set a margin in the plot region.
##'
##' @param x the data vector (either the orginal vector or its range)
##' or a matrix of 2x2 which defines the ranges of two axes in two
##' columns
##' @param f the amount to extend the range
##' @return a vector of length 2: the lower and upper bounds
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples .extend.ranges(c(0, 1))
##' .extend.ranges(1:10)
##' .extend.ranges(matrix(c(c(1,10), c(5, 8)), nrow = 2))
##'
.extend.ranges = function(x, f = qpar('mar')) {
    if (is.matrix(x)) {
        if (!identical(dim(x), c(2L, 2L))) stop('the range matrix must be of dim 2x2')
        x = apply(x, 2, sort)
        return(x + f * t((x[2, ] - x[1, ]) %*% t(c(-1, 1))))
    }
    x = range(x)
    x + c(-1, 1) * f * (x[2] - x[1])
}
