require(cranvas)

data(nasa)
nasa11 <- subset(nasa, Gridx == 1 & Gridy == 1)
qnasa <- qdata(nasa11)
selected(qnasa)[1] <- TRUE

#nasa1 <- subset(nasa, Gridx ==1)
#qnasa1 <- qdata(nasa1)
#qtime(qnasa1,TimeIndx,ts,Gridy)

# qtime(qnasa,TimeIndx,ts)
# qtime(qnasa,TimeIndx,ts,wrap=FALSE)
 qtime(qnasa,TimeIndx,ts,Year)
# qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med))
# qscatter(qnasa,ts,ps_tovs)
# qscatter(qnasa,TimeIndx,tsa_tovs)
