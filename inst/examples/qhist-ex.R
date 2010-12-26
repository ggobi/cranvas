## examples of qhist()

library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)
library(ggplot2)

data(tennis)
qtennis <- qmutaframe(tennis)

qhist(qtennis, "First.Serve.Pct", horizontal=FALSE)
