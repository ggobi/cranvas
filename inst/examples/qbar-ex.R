library(cranvas)
data(nrcstat)
qnrc = qdata(nrcstat, fill = dscale(nrcstat$Regional.Code, hue_pal()), color = dscale(nrcstat$Regional.Code, hue_pal()))
(ob = qbar(Regional.Code, qnrc))
qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", glyph = "tick",
    horizontal = FALSE, boxplot = TRUE)

## automatically brush the bar plot
brush(qnrc, 'style')$color = 'brown'
l = attr(ob, 'meta')$limits
p = approx(runif(50, l[1, 1], l[2, 1]), runif(50, l[1, 2], l[2, 2]), n = 500)  # interpolate some random positions
manual_brush(ob, cbind(p$x, p$y), pause = 0.1)
