## template to generate a brush
brushGen = setRefClass('BRUSH',
              fields = c(signalingField('style', 'list'), # style: color, size, linetype
              signalingField('color', 'character'), # color of brushed elements
              signalingField('color.gen', 'function'), # fun to generate color
              signalingField('size', 'numeric'), # size of brushed elements
              signalingField('size.gen', 'function'), # fun to generate size
              signalingField('mode', 'character'), # brush mode
              signalingField('identify', 'logical'), # whether to identify
              signalingField('label.gen', 'function'), # fun to return texts
              signalingField('label.color', 'character'), # col of texts
              signalingField('history.size', 'numeric'), # history to store
              signalingField('history.index', 'numeric'), # current index
              signalingField('history.list', 'list'), # brush history
              signalingField('persistent', 'logical'), # persistent or transient brushing
              signalingField('persistent.color', 'character'), # persistent colors
              signalingField('persistent.list', 'list'), # persistent brushing history
              signalingField('cursor', 'numeric') # the cursor type
              ))


##' Set or query the brush attributes.
##'
##' @param data the mutaframe created by \code{\link{qdata}},
##' with an attribute \code{Brush}
##' @param attr the name of the brush attribute (a character scalar),
##' e.g. \code{style} (the color, size and linetype of the brush),
##' \code{color} (the color of the objects selected by the brush),
##' \code{size} (the size of the selected objects, e.g. line width or
##' size of points); if \code{attr} is missing, the brush object will
##' be returned
##' @return the brush object or the attribute of the brush; note the
##' brush object can be further manipulated with other methods -- see
##' examples below
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples qiris = qdata(head(iris))
##' brush(qiris)  # all attributes
##' brush(qiris, 'color')
##' brush(qiris, 'color') = 'green'  # set brush color to green
##'
##' ## attach events on the brush
##' b = brush(qiris)
##'
##' #  idx is the index of the event; it can be used to stop the listening
##' idx = b$colorChanged$connect(function() {
##' message('the color of brushed elements was changed to ', b$color)})
##' b$color = 'brown'
##' b$color = 'gold'
##'
##' #  disconnect the event
##' b$colorChanged$disconnect(idx)
##'
brush = function(data, attr) {
    if (!is.mutaframe(data) || !('Brush' %in% names(attributes(data))))
        stop("data must be a mutaframe and have an attribute 'Brush' (typically created by qdata())")
    .brush.attr = base::attr(data, 'Brush')
    if (missing(attr)) {
        .brush.attr
    } else {
        .brush.attr[[attr[1]]]
    }
}

##' @rdname brush
##' @usage brush(data, attr) <- value
##' @param value the value of the brush attribute
##' @export "brush<-"
##' @return set the attribute of the brush
`brush<-` = function (data, attr, value) {
    b = attr(data, "Brush")
    eval(parse(text = paste('b$', attr[1], ' = value', sep = '')))
    data
}
