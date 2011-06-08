# Examples for qdensity
library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)
library(scales)

data(tennis)
qtennis <- qdata(tennis)

print(qdensity(First.Serve.Pct, qtennis))
