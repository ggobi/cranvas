## data from the ASA Data Expo 2006
## http://stat-computing.org/dataexpo/2006/

library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)

data(nasa)
qnasa <- qdata(nasa)

qscatter(data= qnasa, Long, Lat)
qscatter(data= qnasa, TimeIndx, tsa_tovs)
qparallel(data=qnasa)

