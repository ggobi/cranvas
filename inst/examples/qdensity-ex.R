# Examples for qdensity
library(cranvas)

####
data(tennis)
qtennis <- qdata(tennis)

print(qdensity(first.serve.pct, qtennis))
print(qdensity(second.serve.pts, qtennis))
print(qdensity(serve.speed, qtennis))

record_selector("name", qtennis)

####
if (require('animation')) {
  data(pollen, package = 'animation')
  qpollen <- qdata(pollen)
  print(qdensity(RIDGE, data = qpollen))
}

####
data(flea, package = 'tourr')
qflea <- qdata(flea)

print(qdensity(tars1, qflea))
print(qdensity(tars2, qflea))
print(qdensity(aede1, qflea))
print(qdensity(aede3, qflea))

#### test colors
qflea <- qdata(flea, color = species)
print(qdensity(tars1, qflea))
