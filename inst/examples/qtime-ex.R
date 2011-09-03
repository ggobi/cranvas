library(cranvas)

## example 1: NASA temperature data
data(nasa)
nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
qnasa <- qdata(nasa11)
print(qtime(TimeIndx,~ts,qnasa,shift=c(1,12)))
print(qtime(TimeIndx,~ts,qnasa,wrap=FALSE))
print(qtime(TimeIndx,~ts,qnasa,Year,shift=1))
print(qtime(TimeIndx,~ts,qnasa,Year,wrap=FALSE))
print(qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,shift=c(1,12)))
print(qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,wrap=FALSE))
print(qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,Year))
print(qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,Year,wrap=FALSE))

library(reshape)
nasaTsCa <- nasa11[,c(6,9,14)]
nasaTsCa[,2:3] <- rescaler(nasaTsCa[,2:3])
nasaTsCa <- melt(nasaTsCa,1)
qnasaTsCa <- qdata(nasaTsCa)
print(qtime(TimeIndx,~value,qnasaTsCa,group=variable,shift=c(1,12)))


## example 2: Remifentanil in the nlme package
require(nlme)
Rem <- qdata(Remifentanil[complete.cases(Remifentanil) &
                          Remifentanil$ID==1,])
Remi <- Remifentanil[complete.cases(Remifentanil),]
Remi$ID <- factor(Remi$ID)
qRemi <- qdata(Remi)
print(qtime(Time,~conc,Rem))
print(qtime(Time,~conc,qRemi,group=ID))
print(qtime(Time,~conc,qRemi,group=ID,wrap=FALSE))

# for categorical brushing self-link dataset by ID:
link_var(qRemi) <- "ID"   # ON
link_type(qRemi) <- "self"

link_var(qRemi) <- NULL  # OFF

                          
## example 3: Wages
data(wages)
wage <- qdata(wages[as.integer(as.character(wages$id))<2000,1:3])
print(qtime(exper,~lnw,wage,group=id))
link_var(wage) <- "id"   # ON
link_type(wage) <- "self"
link_var(wage) <- NULL  # OFF


## example 4: Lynx - for posterity
# Good to show off wrapping to investigate irregular series
data(lynx)
qlynx<-qdata(data.frame(Time=1:114, lynx))
print(qtime(Time, ~lynx, qlynx, shift=1:12))


## example 5: Sunspots - for posterity
# Good to show off wrapping to investigate irregular series
data(sunspots)
qsun<-qdata(data.frame(Time=1:2820, sunspots))
print(qtime(Time, ~sunspots, qsun, shift=c(1,(1:10)*10)))


## example 6: Pigs
data(pigs)
qpig<-qdata(pigs)
print(qtime(TIME, ~GILTS+PROFIT+PRODUCTION+HERDSZ, qpig, shift=c(1,4)))

library(reshape)
pigGP <- pigs[,c(1,7,8)]
pigGP[,2:3] <- rescaler(pigGP[,2:3])
pigGP <- melt(pigGP,1)
qpigGP <- qdata(pigGP)
print(qtime(TIME,~value,qpigGP,group=variable,shift=c(1,4)))

link_var(qpigGP) <- "variable"   # ON
link_type(qpigGP) <- "self"
