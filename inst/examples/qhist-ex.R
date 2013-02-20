library(cranvas)

#### (1) split histogram by species, and categorical linking
data(flea, package = 'tourr')
qflea = qdata(flea, color = species)

qhist(tars1, data = qflea)
qhist(tars2, data = qflea, freq = FALSE, main = 'density of tars2')
qhist(aede1, data = qflea, ylim = c(0, 10), main = 'fix y-axis limits')
qhist(head, data = qflea, horizontal = TRUE)
qspine(aede2, data = qflea)
qparallel(~., data = qflea)  # all variables

id = link_cat(qflea, 'species')  # linking by categorical variable
## now brush one bar, all fleas of the same species will be brushed


## remove linking
remove_link(qflea, id)


#### (2) tennis data: without a splitting variable

qtennis = qdata(tennis, color = 'white', border = 'black')

qhist(first.serve.pct, data = qtennis)
qbar(matches, data = qtennis)
qspine(matches, data = qtennis)

qhist(first.serve.pct, data = qtennis, horizontal = TRUE)

qhist(serve.speed, data = qtennis)
qhist(serve.speed, data = qtennis, freq = FALSE)  # density


#### see help(wages) for yet another example
