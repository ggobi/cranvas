
### (1) linking to oneself
data(flea, package = 'tourr')
qflea = qdata(flea, color = species)
qscatter(tars1, tars2, data = qflea)

## brush the nearest 10 points around the center
id = link_knn(qflea, c('tars1', 'tars2'), k = 10)

## remove the linking
remove_link(qflea, id)


### (2) variables on the same scale
mf = qdata(data.frame(x = rnorm(300), y = rnorm(300)), size = 3)
qscatter(x, y, data = mf)

id = link_knn(mf, c('x', 'y'), k = 20)

remove_link(mf, id)


### (3) link two datasets

mf1 = qdata(flea, color = species)
mf2 = qdata(subset(flea, species == 'Concinna '))

qscatter(tars1, tars2, data = mf1)
qscatter(tars1, tars2, data = mf2)

id = link_knn(mf1, c('tars1', 'tars2'), mf2, c('tars1', 'tars2'))

remove_link(mf1, id[1])
remove_link(mf2, id[2])

cranvas_off()
