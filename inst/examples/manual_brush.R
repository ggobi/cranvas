library(cranvas)
data(nrcstat)
qnrc = qdata(nrcstat, fill = dscale(nrcstat$Regional.Code, hue_pal()), color = dscale(nrcstat$Regional.Code, hue_pal()))
(o1 = qbar(Regional.Code, qnrc))
(o2 = qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", glyph = "tick",
    horizontal = FALSE, boxplot = TRUE))

## automatically brush the bar plot
brush(qnrc, 'style')$color = 'brown'
l = attr(o1, 'meta')$limits
p = approx(runif(50, l[1, 1], l[2, 1]), runif(50, l[1, 2], l[2, 2]), n = 500)  # interpolate some random positions
manual_brush(o1, cbind(p$x, p$y), pause = 0.1)

## can also brush the par-coords plot
manual_brush(o2, as.matrix(expand.grid(seq(0, 1, 0.1), (1:4)-.1)), pause = 0.2)
