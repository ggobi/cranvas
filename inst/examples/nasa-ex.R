
library(cranvas)
qnasa <- qdata(nasa)

qscatter(Long, Lat, data = qnasa)
qscatter(TimeIndx, tsa_tovs, data = qnasa)
qparallel(data = qnasa)

cranvas_off()
