library(cranvas)
library(qtpaint)

s = qscene()
r = qlayer(s)
r[1, 1] = qlayer(paintFun = function(layer, painter) {
qdrawCircle(painter, runif(1000, 0, 10), runif(1000, -5, 3), r = 2)
qdrawRect(painter, 0, -5, 10, 3)
}, limits = qrect(c(0, 10), c(-5, 3))) # main layer
## note the vertical limits of x-axis and horizontal limits of y-axis are [0, 1]
#' x-axis
r[2, 1] = qaxis(side = 1, at = c(0, 1, 3, 7, 8), limits = qrect(c(0, 10), c(0, 1)))
#' y-axis
r[1, 0] = qaxis(side = 2, at = c(-4.5, -1, 0, 1.5, 2.5), limits = qrect(c(0, 1), c(-5, 3)))
#' top x-axis
r[0, 1] = qaxis(side = 3, meta = list(xat = c(1, 3, 7), xlabels = c('a', 'b', 'c'), limits = matrix(c(0, 10, -5, 3), 2)))

fix_dimension(r, row = list(id = c(0, 2), value = c(30, 30)),
              column = list(id = 0, value = 30))

qplotView(scene = s)

cranvas_off()
