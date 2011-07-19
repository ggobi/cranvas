library(cranvas)

## example 1: NASA temperature data
data(nasa)
nasa11 <- subset(nasa, Gridx == 1 & Gridy == 1)
qnasa <- qdata(nasa11)
print(qtime(qnasa,TimeIndx,ts))
print(qtime(qnasa,TimeIndx,ts,wrap=FALSE))
print(qtime(qnasa,TimeIndx,ts,Year))
print(qtime(qnasa,TimeIndx,ts,Year,wrap=FALSE))
print(qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med)))
print(qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med),wrap=FALSE))
print(qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med),Year))
print(qtime(qnasa,TimeIndx,c(ts,ps_tovs,ca_med),Year,wrap=FALSE))


## example 2: Norway precipitation data
data(norwayprecip)
nwp <- qdata(data.grid.wk)
print(qtime(nwp,time,obs,period=year))
print(qtime(nwp,time,obs,period=year,wrap=FALSE))


## example 3: Remifentanil in the nlme package
require(nlme)
Rem <- qdata(Remifentanil[complete.cases(Remifentanil) &
                          Remifentanil$ID==1,])
Remi <- qdata(Remifentanil[complete.cases(Remifentanil),])
Remi$ID <- factor(Remi$ID)
print(qtime(Rem,Time,conc))
print(qtime(Remi,Time,conc,group=ID))
print(qtime(Remi,Time,conc,group=ID,wrap=FALSE))

# for categorical brushing self-link dataset by ID:
link_var(Remi) <- "ID"   # ON
link_type(Remi) <- "self"

link_var(Remi) <- NULL  # OFF

## example 4: Wages data
data(wages)
wage <- qdata(wages[,1:3])
wage31 <- qdata(wages[wages$id==31,1:3])
print(qtime(wage31,exper,lnw))
print(qtime(wage,exper,lnw,group=id))

