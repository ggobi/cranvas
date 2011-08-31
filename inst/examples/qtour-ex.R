library(cranvas)
data(flea, package = 'tourr')

qflea = qdata(flea, color = species)
flea_tour = qtour(1:6, data = qflea, tour_path = grand_tour(3))
flea_tour$start()

qparallel(~tour_1+tour_2+tour_3)
qhist(tour_1, binwidth = 0.05, xlim = c(-1, 1), ylim = c(0, 16))

flea_tour$pause()

## adjust speed
flea_tour$start()
flea_tour$slower()
flea_tour$faster()

## change the type of tour
flea_tour$tour_path = guided_tour(holes, d = 3)

## do sphering
flea_tour$sphere = TRUE
