library(cranvas)

### (1) ames housing data
qames <- qdata(ameshousing)

qdensity(saleprice, data = qames)

### (2) tennis data
qtennis <- qdata(tennis)

qdensity(first.serve.pct, data = qtennis)

qdensity(second.serve.pts, data = qtennis)
qdensity(serve.speed, data = qtennis)

record_selector(name, data = qtennis)

### (3) pollen data
if (require('animation')) {
  data(pollen, package = 'animation')
  qpollen <- qdata(pollen)
  print(qdensity(RIDGE, data = qpollen))
}

### (4) flea (with colors)
data(flea, package = 'tourr')
qflea <- qdata(flea, color = species)

qdensity(tars1, data = qflea)

qdensity(tars2, data = qflea)
qdensity(aede1, data = qflea)
qdensity(aede3, data = qflea)

#### test colors
qflea <- qdata(flea, color = species)
qdensity(tars1, data = qflea)
