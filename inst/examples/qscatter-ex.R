library(cranvas)

### (1) tennis data
qtennis <- qdata(tennis)

qscatter(first.serve.pct, second.serve.pts, data=qtennis, xlab="First Serve %", ylab="Second Serve Points")
qscatter(return.games, first.serves, data=qtennis)

### (2) flea: color by categorical variable, and linking
data(flea, package = 'tourr')
qflea <- qdata(flea, color = species)  # use species to create colors

qscatter(tars1, aede1)
qscatter(tars2, head)

qscatter(tars1, tars2, asp = .7)  # aspect ratio

qscatter(tars1, tars2, unibrushcolor=FALSE)  # brush color

## link qflea to itself using species
id = link_cat(qflea, 'species')

## remove linking
remove_link(qflea, id)

## a bubble chart
qflea2 = qdata(flea, color = NA, border = species, size = tars1)
qscatter(tars1, tars2)

### (3) NRC rankings

qnrc = qdata(nrcstat, color = RegCode)

qscatter(RRankings5th, RRankings95th)

qscatter(SRankings5th, SRankings95th)

qscatter(SRankings5th, SRankings95th, unibrushcolor=FALSE)

### (4) secrets in the pollen data
library(animation)
data(pollen, package = 'animation')
head(pollen)
qpollen = qdata(pollen, size = 2)
qscatter(RIDGE, CRACK, data = qpollen)
## try zooming into the center or press +/-

### (5) pressure test; run with care!
n = 1e+06  # a million still works (at least for me)
df = qdata(data.frame(x = rnorm(n), y = rnorm(n),
    z = gl(4, n/4)), color = z)
qscatter(x, y)

