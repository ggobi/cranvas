library(cranvas)
data(nrcstat)
qnrc = qdata(nrcstat, color = Regional.Code)
qbar(Regional.Code, qnrc)
qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", glyph = "tick",
    horizontal = FALSE, boxplot = TRUE)

qbar(Regional.Code, qnrc, horizontal = TRUE)  # horizontal plot
