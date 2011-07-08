# Examples for qdensity
library(cranvas)

data(tennis)
qtennis <- qdata(tennis)

print(qdensity(First.Serve.Pct, qtennis))
print(qdensity(Second.Serve.Pts, qtennis))

data(pollen)
qpollen <- qdata(pollen)
print(qdensity(RIDGE, qpollen))

data(flea)
qflea <- qdata(flea)

print(qdensity(tars1, qflea))
print(qdensity(tars2, qflea))
print(qdensity(aede1, qflea))
print(qdensity(aede3, qflea))
