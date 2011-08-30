##' @rdname qhist
##' @export
qspine = function(x, data = last_data(), ...) {
    do.call(qhist, list(x = as.character(as.list(match.call()[-1])$x),
                        data = data, spine = TRUE, ...))
}
