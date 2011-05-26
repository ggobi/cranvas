require(cranvas)

data(nasa)
nasa11 <- subset(nasa, Gridx == 1 & Gridy == 1)
qnasa <- qdata(nasa11)
selected(qnasa)[1] <- TRUE

qtime(qnasa,TimeIndx,ts)
qscatter(qnasa,ts,tsa_tovs)

qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med))
