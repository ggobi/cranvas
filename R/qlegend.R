##' Create a legend layer
##'
##' This function creates a legend layer.
##' @param parent the parent layer (default to be \code{NULL}, which
##' means creating an independent layer with no parents, but it can be
##' added to a parent layer using the approach \code{parent[i, j] <-
##' child_layer})
##' @param data \code{NULL} means to use \code{scale}
##' @param vertical (default is \code{TRUE})
##' @param ... other arguments passed to \code{\link[qtpaint]{qlayer}}
##' @return a layer object
##' @author Heike Hofmann
##' @export
qlegend = function(parent = NULL, data = NULL, vertical = TRUE, ...) {
    lims <- qrect(c(0,1),c(0,1))

    draw_legend = function(layer, painter) {
        scale <- attr(data,"col.scale")
        if (is.null(scale)) return (NULL)
				xpos = if (vertical) 0 else 0.6
        ypos = if (vertical) 0.6 else 0

       qstrokeColor(painter) <- "black"
        qdrawText(painter, scale$name, xpos, ypos, valign = "top",
            halign = "left")
        fontHeight <- qstrHeight(painter, scale$name)
        # get 10 by 10 pixel rectangles for fill legend
        x <- c(0,10/layer$size$width())
        y <- c(0,10/layer$size$height())

        # create a set of rectangles
        r0 <- c(0, 0, diff(range(x)), diff(range(y)))
        ypos <- ypos - 3 * qstrHeight(painter, scale$name)
        r0 <- r0 + c(xpos, ypos)

        d <- options()$str$digits.d
        #   browser()
        qval <- seq(scale$limits[1],scale$limits[2],length=5)
        qcol <- scale$map(qval)


        for (i in 1:length(qcol)) {
            qdrawRect(painter, r0[1], r0[2], r0[3], r0[4], fill = qcol[i],
            	stroke = "black")
            qdrawText(painter, as.character(round(qval[i],d)), xpos + 1.5 * (r0[3] - r0[1]),
                ypos + fontHeight, valign = "top", halign = "left")
            ypos <- ypos - 1.5 * fontHeight
            r0[2] <- r0[2] - 1.5 * fontHeight
            r0[4] <- r0[4] - 1.5 * fontHeight
        }


    }
        qlayer(parent, paintFun = draw_legend, limits = lims, ...)
}

