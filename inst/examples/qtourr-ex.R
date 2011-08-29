## examples of tourrs in cranvas
library(cranvas)

data(flea, package = 'tourr')
flea.s <- flea
flea.s[, -7] <- tourr::rescale(flea.s[, -7])
qflea <- qdata(flea.s)
flea_tour <- Tourr$new(qflea, grand_tour(3), 1:6)
flea_tour$step()

qparallel(c("tour_1", "tour_2", "tour_3"), qflea)
qscatter(tour_1, tour_2, qflea, labeled=TRUE, xlim=c(-4,4), ylim=c(-4,4))
## buggy: qhist(tour_1, qflea)
qdensity(tour_1, qflea, xlim=c(-4,4))

flea_tour$start()
flea_tour$pause()
flea_tour$slower()
flea_tour$faster()
