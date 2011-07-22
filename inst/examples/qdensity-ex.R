# Examples for qdensity
library(cranvas)

####
data(tennis)
qtennis <- qdata(tennis)

print(qdensity(First.Serve.Pct, qtennis))
print(qdensity(Second.Serve.Pts, qtennis))
print(qdensity(Serve.Speed, qtennis))

record_selector("Name", qtennis)

####
data(pollen)
qpollen <- qdata(pollen)
print(qdensity(RIDGE, qpollen))

####
data(flea)
qflea <- qdata(flea)

print(qdensity(tars1, qflea))
print(qdensity(tars2, qflea))
print(qdensity(aede1, qflea))
print(qdensity(aede3, qflea))

#### test colors
qflea <- qdata(flea, fill = dscale(flea$species, hue_pal()), color = dscale(flea$species, hue_pal()))
print(qdensity(tars1, qflea))
