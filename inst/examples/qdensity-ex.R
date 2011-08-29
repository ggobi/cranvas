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
if (require('animation')) {
  data(pollen, package = 'animation')
  qpollen <- qdata(pollen)
  print(qdensity(RIDGE, data = qpollen))
}

####
data(flea)
qflea <- qdata(flea)

print(qdensity(tars1, qflea))
print(qdensity(tars2, qflea))
print(qdensity(aede1, qflea))
print(qdensity(aede3, qflea))

#### test colors
qflea <- qdata(flea, color = species)
print(qdensity(tars1, qflea))
