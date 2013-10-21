library(cranvas)
str(tennis)
## more Aces, closer to red; less, blue; higher speed, larger points
qtennis = qdata(tennis, color = aces, size = serve.speed)

qhist(aces, data = qtennis, main = 'Number of Aces')
ls.str(attr(qtennis, 'Scales'))  # the scales information

selected(qtennis)[1:10] = TRUE  # brush the first 10 cases

b = brush(qtennis)  # the brush object
b$style  # style the brush rectangle
b$style$color = 'brown'  # a brown brush

b$color  # color of brushed elements

b$color = 'cyan'  # brushed elements become cyan


attr(qtennis, 'Shadow')  # should be NULL, since no misssing values here


## we can also use the default dark gray
qtennis = qdata(tennis)
qhist(double.faults, data = qtennis)

cranvas_off()
