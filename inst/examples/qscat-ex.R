library(cranvas)
library(scales)

data(nrcstat)
nrcstat[,26]<- -nrcstat[,26]
colnames(nrcstat)[26]<-"Neg.Median.Time.to.Degree"

qnrc = qdata(nrcstat)

print(qscatter(R.Rankings.5th.Percentile, R.Rankings.95th.Percentile, qnrc))
print(qscatter(S.Rankings.5th.Percentile, S.Rankings.95th.Percentile, qnrc))

data(pollen)
qpollen <- qdata(pollen)
qscatter(RIDGE, CRACK, qpollen)
# try zooming into the center

# categorical variable linking
data(flea)
qflea <- qdata(flea)
print(qscatter(tars1, aede1, qflea))

link_var(qflea) = 'species'
link_type(qflea) = 'self'

# Using species to color points
qflea <- qdata(flea, fill = dscale(flea$species, hue_pal()), color = dscale(flea$species, hue_pal()))
print(qscatter(tars1, aede1, qflea))

