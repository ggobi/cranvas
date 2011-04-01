data(nasa)
nasa11 <- subset(nasa, Gridx==1 & Gridy==1)
qnasa <- qdata(nasa11)
# selected(qnasa)[1] <- TRUE
qts("TimeIndx", "ts", qnasa) 
