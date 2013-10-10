library(cranvas)
library(reshape)
library(tourr)
data(flea, package = 'tourr')

qflea = qdata(flea, color = species)
flea_tour = qtour(1:6, data = qflea, tour_path = grand_tour(3))
flea_tour$start()

Sys.sleep(.5)  # need a pause to generate projections
qscatter(proj1, proj2, data = qflea, xlim = c(-.8, .8), ylim = c(-.8, .8), xlab="Projection 1", ylab="Projection 2")
qparallel(~proj1+proj2+proj3, data = qflea)
qhist(proj1, data = qflea, binwidth = 0.05, xlim = c(-1, 1), ylim = c(0, 10))

flea_tour$pause()

## adjust speed
flea_tour$start()
flea_tour$setSpeed(2)   # speed up (twice as fast)
flea_tour$setSpeed(.5)  # slow down
flea_tour$faster()
flea_tour$slower()

## change the type of tour
flea_tour$tour_path = guided_tour(holes, d = 3)
flea_tour$tour_path = grand_tour(d = 3)
flea_tour$tour_path = guided_tour(lda_pp(flea$species), d = 3)
flea_tour$tour_path = grand_tour(d = 3)

## do sphering
flea_tour$sphere = TRUE

