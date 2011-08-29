library(cranvas)

#### (1) split histogram by species, and categorical linking
data(flea, package = 'tourr')
qflea = qdata(flea, color = species)

qhist(tars1)
qhist(tars2, freq = FALSE, main = 'density of tars2')
qhist(aede1)
qhist(head, horizontal = TRUE)
qspine(aede2)
qparallel(~.)  # all variables

link_var(qflea) = 'species'  # linking by categorical variable
link_type(qflea) = 'self'
## now brush one bar, all fleas of the same species will be brushed

## remove linking
link_var(qflea) = NULL


#### (2) tennis data: without a splitting variable
data(tennis)

qtennis = qdata(tennis, color = 'white', border = 'black')

qhist(First.Serve.Pct)
qbar(Matches)
qspine(Matches)

qhist(First.Serve.Pct, horizontal = TRUE)

qhist(Serve.Speed)
qhist(Serve.Speed, freq = FALSE)  # density


#### see help(wages) for yet another example
