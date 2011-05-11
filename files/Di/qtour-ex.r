library(cranvas)
library(tourr)

flea<-rescaler(flea)
qflea<-qdata(flea)
qtour_xy(qflea[,1:6])
