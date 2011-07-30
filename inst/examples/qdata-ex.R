library(cranvas)
data(tennis)
str(tennis)
qtennis = qdata(tennis, color = Aces, fill = Aces)  # more Aces, closer to red; less, blue

qhist2(Aces, data = qtennis, main = 'Number of Aces')
attr(qtennis, 'Scales')  # the scales information

selected(qtennis)[1:10] = TRUE  # brush the first 10 cases

b = brush(qtennis)  # the brush object
b$style  # style the brush rectangle
b$style$color = 'brown'  # a brown brush

b$color  # color of brushed elements

b$color = 'cyan'  # brushed elements become cyan

## linking by categorical variable Country
link_var(qtennis) = 'Country'
link_type(qtennis) = 'self'
## now all elements in the same category as those brushed will be brushed

attr(qtennis, 'Shadow')  # should be NULL, since no misssing values here


## we can also use the default black color and gray fill
qtennis = qdata(tennis)
qhist2(Double.Faults, data = qtennis)
