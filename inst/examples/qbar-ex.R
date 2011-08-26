library(cranvas)
data(nrcstat)
qnrc = qdata(nrcstat, color = Regional.Code)
qbar(Regional.Code)
qparallel(vars = 13:10, main = "Overview of Rankings", glyph = "tick",
    horizontal = FALSE, boxplot = TRUE)

qbar(Regional.Code, horizontal = TRUE)  # horizontal plot
