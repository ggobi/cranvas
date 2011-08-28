##' @rdname qhist
##' @export
qspine = function(x, data = last_data(), breaks = 30, main = '', horizontal = FALSE) {
    do.call(qhist, list(x = as.character(as.list(match.call()[-1])$x),
                        data = data, breaks = breaks, main = main,
                        horizontal = horizontal, spine = TRUE))
}
