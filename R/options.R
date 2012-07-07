#' Graphical parameters in cranvas
#'
#' This function can set or query the graphical parameters in the \pkg{cranvas}
#' package.
#'
#' Current options include:
#'
#' \describe{
#'
#' \item{cache}{whether to enable cache for graphics layers}
#'
#' \item{mar}{a percentage for the plot margin}
#'
#' }
#' @param ... options of the form \code{tag = value}
#' @return the current list of parameters or set new options
#' @author Yihui Xie <\url{http://yihui.name}>
#' @seealso \code{\link[graphics]{par}}
#' @export
#' @examples
#' str(qpar())  # all default parameters
#' op = qpar()
#' qpar(mar = 0.05)  # the degree to extend the plot margin (inner margin)
#' qpar(op)  # restore
qpar = function(...) {
  p = list(...); opts = .cranvasEnv$.opts
  # get all options
  if (!length(p)) return(opts)
  # get specified options
  if (is.null(names(p)) && !is.list(p[[1]])) {
    p = unlist(p)
    if (length(p) == 1L) opts[[p]] else opts[p]
  } else {
    if (is.list(p[[1]])) p = p[[1]]
    .cranvasEnv$.opts[names(p)] = p
  }
}

qpar(mar = 0.05, cache = FALSE)

# a convenience function
.cache = function() qpar('cache')
