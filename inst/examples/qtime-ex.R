require(cranvas)

## example 1: NASA temperature data
data(nasa)
nasa11 <- subset(nasa, Gridx == 1 & Gridy == 1)
qnasa <- qdata(nasa11)
selected(qnasa)[1] <- TRUE

# qtime(qnasa,TimeIndx,ts)
# qtime(qnasa,TimeIndx,ts,wrap=FALSE)
# qtime(qnasa,TimeIndx,ts,Year)
# qtime(qnasa,TimeIndx,ts,Year,wrap=FALSE)
 qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med))
# qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med),wrap=FALSE)


## example 2: Norway precipitation data
data(norwayprecip)
nwp <- qdata(data.grid.wk)
# qtime(nwp,time,obs,year)
# qtime(nwp,time,obs,year,wrap=FALSE)

