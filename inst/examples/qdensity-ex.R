# Examples for qdensity
library(cranvas)

data(tennis)
qtennis <- qdata(tennis)

print(qdensity(First.Serve.Pct, qtennis))
print(qdensity(Second.Serve.Pts, qtennis))

data(pollen)
qpollen <- qdata(pollen)
print(qdensity(RIDGE, qpollen))
