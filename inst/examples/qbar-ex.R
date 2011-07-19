library(cranvas)
data(nrcstat)
qnrc = qdata(nrcstat, fill = dscale(qnrc$Regional.Code, brewer_pal(type = 'qual')))
qbar(Regional.Code, qnrc)
qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", glyph = "tick",
    horizontal = FALSE, boxplot = TRUE)
