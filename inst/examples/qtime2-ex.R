library(cranvas)

## example 1: NASA temperature data
data(nasa)
nasa2221 <- subset(nasa, Gridx == 22 & Gridy == 21)
nasa2221$Year <- factor(nasa2221$Year)
qnasa <- qdata(nasa2221)
qnasa1 <- time_qdata(qnasa,"ts")
qnasa2 <- time_qdata(qnasa,c("ts","ps_tovs","ca_med"))

a=qtime2(TimeIndx,qnasa1,shift=c(1,12))
meta=attr(a,'meta')
a

qtime2(TimeIndx,qnasa1,Year,shift=1)

qtime2(TimeIndx,qnasa2,shift=c(1,12))

qtime2(TimeIndx,qnasa2,Year)


## example 2: Remifentanil in the nlme package
library(nlme)
qRem <- time_qdata(Remifentanil[complete.cases(Remifentanil) &
    Remifentanil$ID==1,],"conc")
qtime2(Time,qRem)

Remi <- Remifentanil[complete.cases(Remifentanil),]
Remi$ID <- factor(Remi$ID)
qRemi <- time_qdata(Remi,"conc")
qtime2(Time,qRemi,group=ID)
# for categorical brushing self-link dataset by ID:
# id <- link_cat(qRemi, "ID")
# remove_link(qRemi, id)


## example 3: Wages
data(wages)
qwage <- time_qdata(wages[as.integer(as.character(wages$id))<2000,1:3],"lnw")
a=qtime2(exper,qwage,group=id)
# id <- link_cat(wage, "id")
# remove_link(wage, id)


## example 4: Lynx - for posterity
# Good to show off wrapping to investigate irregular series
data(lynx)
qlynx<-time_qdata(data.frame(Time=1:114, lynx),"lynx")
qtime2(Time, qlynx, shift=1:12)


## example 5: Sunspots - for posterity
# Good to show off wrapping to investigate irregular series
data(sunspots)
qsun<-time_qdata(data.frame(Time=1:2820, sunspots),"sunspots")
qtime2(Time, qsun, shift=c(1,(1:10)*10))


## example 6: Pigs
data(pigs)
qpig<-time_qdata(pigs,c("GILTS","PROFIT","PRODUCTION","HERDSZ"))
qtime2(TIME, qpig, shift=c(1,4))
