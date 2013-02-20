library(cranvas)

## example 1: NASA temperature data
nasa2221 <- subset(nasa, Gridx == 22 & Gridy == 21)
nasa2221$Year <- factor(nasa2221$Year)
qnasa <- qdata(nasa2221)
qnasa1 <- time_qdata(qnasa,"ts")
qnasa2 <- time_qdata(qnasa,c("ts","ps_tovs","ca_med"))

qtime(TimeIndx,qnasa1,shift=c(1,12))
qscatter(data=qnasa,ts,ps_tovs)

qtime(TimeIndx,qnasa1,Year,shift=1)
qtime(TimeIndx,qnasa2,shift=c(1,12))
qtime(TimeIndx,qnasa2,Year)


## example 2: Remifentanil in the nlme package
library(nlme)
qRem <- qdata(Remifentanil[complete.cases(Remifentanil) & Remifentanil$ID==1,])
qRem_time <- time_qdata(qRem,"conc")
qtime(Time,qRem_time)

Remi <- Remifentanil[complete.cases(Remifentanil),]
Remi$ID <- factor(Remi$ID)
qRemi <- qdata(Remi)
qRemi_time <- time_qdata(qRemi,"conc")
qtime(Time,qRemi_time,group=ID)
# for categorical brushing self-link dataset by ID:
# id <- link_cat(qRemi, "ID")
# remove_link(qRemi, id)


## example 3: Wages
qwage <- qdata(wages[as.integer(as.character(wages$id))<2000,1:3])
qwage_time <- time_qdata(qwage, "lnw")
qtime(exper,qwage_time,group=id)
# id <- link_cat(wage, "id")
# remove_link(wage, id)


## example 4: Lynx - for posterity
# Good to show off wrapping to investigate irregular series
qlynx <- qdata(data.frame(Time=1:114, lynx))
qlynx_time <- time_qdata(qlynx,"lynx")
qtime(Time, qlynx_time, shift=1:12)


## example 5: Sunspots - for posterity
# Good to show off wrapping to investigate irregular series
qsun <- qdata(data.frame(Time=1:2820, sunspots))
qsun_time <- time_qdata(qsun,"sunspots")
qtime(Time, qsun_time, shift=c(1,(1:10)*10))


## example 6: Pigs
qpig <- qdata(pigs)
qpig_time <- time_qdata(qpig,c("GILTS","PROFIT","PRODUCTION","HERDSZ"))
qtime(TIME, qpig_time, shift=c(1,4))
