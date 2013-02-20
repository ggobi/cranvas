## data from the ASA Data Expo 2006
## http://stat-computing.org/dataexpo/2006/
library(cranvas)

qnasa <- qdata(nasa)

qscatter(Long, Lat, data = qnasa)
qscatter(TimeIndx, tsa_tovs, data = qnasa)
qparallel(data = qnasa)
 
