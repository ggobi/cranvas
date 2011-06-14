# Examples for qdensity
library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)
library(scales)

x<-data.frame(x=c(1:10),y=c(1:10))
qx<-qdata(x)
print(qdensity(x, qx))

data(tennis)
qtennis <- qdata(tennis)

print(qdensity(First.Serve.Pct, qtennis))
