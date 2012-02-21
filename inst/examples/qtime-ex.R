library(cranvas)

## example 1: NASA temperature data
data(nasa)
nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
qnasa <- qdata(nasa11)
qtime(TimeIndx,~ts,qnasa,shift=c(1,12))
qtime(TimeIndx,~ts,qnasa,wrap=FALSE)
qtime(TimeIndx,~ts,qnasa,Year,shift=1)
qtime(TimeIndx,~ts,qnasa,Year,wrap=FALSE)
qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,shift=c(1,12))
qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,wrap=FALSE)
qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,Year)
qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,Year,wrap=FALSE)

library(reshape)
nasaTsCa <- nasa11[,c(6,9,14)]
nasaTsCa[,2:3] <- rescaler(nasaTsCa[,2:3])
nasaTsCa <- melt(nasaTsCa,1)
qnasaTsCa <- qdata(nasaTsCa)
qtime(TimeIndx,~value,qnasaTsCa,group=variable,shift=c(1,12))


## example 2: Remifentanil in the nlme package
require(nlme)
Rem <- qdata(Remifentanil[complete.cases(Remifentanil) &
                          Remifentanil$ID==1,])
Remi <- Remifentanil[complete.cases(Remifentanil),]
Remi$ID <- factor(Remi$ID)
qRemi <- qdata(Remi)
qtime(Time,~conc,Rem)
qtime(Time,~conc,qRemi,group=ID)
qtime(Time,~conc,qRemi,group=ID,wrap=FALSE)

# for categorical brushing self-link dataset by ID:
id <- link_cat(qRemi, "ID")
# remove_link(qRemi, id)


## example 3: Wages
data(wages)
wage <- qdata(wages[as.integer(as.character(wages$id))<2000,1:3])
qtime(exper,~lnw,wage,group=id)
id <- link_cat(wage, "id")
remove_link(wage, id)


## example 4: Lynx - for posterity
# Good to show off wrapping to investigate irregular series
data(lynx)
qlynx<-qdata(data.frame(Time=1:114, lynx))
qtime(Time, ~lynx, qlynx, shift=1:12)


## example 5: Sunspots - for posterity
# Good to show off wrapping to investigate irregular series
data(sunspots)
qsun<-qdata(data.frame(Time=1:2820, sunspots))
qtime(Time, ~sunspots, qsun, shift=c(1,(1:10)*10))


## example 6: Pigs
data(pigs)
qpig<-qdata(pigs)
qtime(TIME, ~GILTS+PROFIT+PRODUCTION+HERDSZ, qpig, shift=c(1,4))

library(reshape)
pigGP <- pigs[,c(1,7,8)]
pigGP[,2:3] <- rescaler(pigGP[,2:3])
pigGP <- melt(pigGP,1)
qpigGP <- qdata(pigGP)
qtime(TIME,~value,qpigGP,group=variable,shift=c(1,4))
id <- link_cat(qpigGP, "variable")
# remove_link(qpigGP, id)

