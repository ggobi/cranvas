library(qtpaint)
## a faceting demo with scatterplots
facet_demo = function(data, facet) {
    facet = eval(substitute(facet), data, parent.frame())
    # need a categorical variable
    stopifnot(is.factor(facet))
    lvl = levels(facet)
    nl = length(lvl)
    # convert to integers
    intFacet = as.integer(facet)
    # range of data
    xmin = min(data[, 1])
    xmax = max(data[, 1])
    ymin = min(data[, 2])
    ymax = max(data[, 2])
    # axis limits
    xlim = c(xmin, xmax) + 0.1 * c(-1, 1) * (xmax - xmin)
    ylim = c(ymin, ymax) + 0.1 * c(-1, 1) * (ymax - ymin)
    # title
    main = deparse(substitute(data))
    scene = qscene()
    root = qlayer(scene)
    qlayer(root, function(item, painter) {
        qdrawText(painter, main, 0.5, 0.5)
    }, limits = qrect(0, 0, 1, 1), row = 0, col = 1)
    # y and x axis
    for (i in 1:nl) {
        qlayer(root, function(item, painter) {
            qdrawText(painter, format(pretty(data[, 2])), 9,
                pretty(data[, 2]), "right", "center")
        }, limits = qrect(c(0, 10), ylim), clip = FALSE, row = i,
            col = 0)
    }
    qlayer(root, function(item, painter) {
        qdrawText(painter, format(pretty(data[, 1])), pretty(data[,
            1]), 9, "center", "top")
    }, limits = qrect(xlim, c(0, 10)), clip = FALSE, row = nl +
        1, col = 1)
    # scatter plots and strips
    for (i in 1:nl) {
        # subset the data according to the faceting variable
        df = subset(data, intFacet == i)
        # scatter plot
        qlayer(root, function(item, painter) {
            qdrawCircle(painter, df[, 1], df[, 2], 2, stroke = "black",
                fill = "black")
            qdrawRect(painter, xlim[1], ylim[1], xlim[2], ylim[2],
                stroke = "black", fill = NA)
        }, limits = qrect(xlim, ylim), clip = FALSE, row = i,
            col = 1)
        # strip labels
        qlayer(root, function(item, painter) {
            qdrawRect(painter, 0, 0, 1, 1, fill = "gray80")
            qdrawText(painter, lvl[i], 0.5, 0.5, rot = 270)
        }, limits = qrect(0, 0, 1, 1), row = i, col = 2)
    }
    layout = root$gridLayout()
    layout$setRowStretchFactor(0, 1)
    for (i in 1:nl) layout$setRowStretchFactor(i, 5)
    layout$setRowStretchFactor(nl + 1, 1)
    layout$setColumnStretchFactor(0, 1)
    layout$setColumnStretchFactor(1, 5)
    layout$setColumnStretchFactor(2, 1)

    view = qplotView(scene = scene)
    view

}
facet_demo(iris, Species)
facet_demo(mtcars,factor(am))
