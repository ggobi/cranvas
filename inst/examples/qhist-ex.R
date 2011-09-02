library(cranvas)

#### (1) split histogram by species, and categorical linking
data(flea, package = 'tourr')
qflea = qdata(flea, color = species)

qhist(tars1)
qhist(tars2, freq = FALSE, main = 'density of tars2')
qhist(aede1, ylim = c(0, 10), main = 'fix y-axis limits')
qhist(head, horizontal = TRUE)
qspine(aede2)
qparallel(~.)  # all variables

id = link_cat(qflea, 'species')  # linking by categorical variable
## now brush one bar, all fleas of the same species will be brushed


## remove linking
remove_listener(qflea, id)


#### (2) tennis data: without a splitting variable
data(tennis)

qtennis = qdata(tennis, color = 'white', border = 'black')

qhist(first.serve.pct)
qbar(matches)
qspine(matches)

qhist(first.serve.pct, horizontal = TRUE)

qhist(serve.speed)
qhist(serve.speed, freq = FALSE)  # density


#### see help(wages) for yet another example
