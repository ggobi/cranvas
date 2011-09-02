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
qRemi$ID <- factor(qRemi$ID,levels=65:1)
qRemi <- qdata(Remi)
print(qtime(Time,~conc,Rem))
print(qtime(Time,~conc,qRemi,group=ID))
print(qtime(Time,~conc,qRemi,group=ID,wrap=FALSE))

# for categorical brushing self-link dataset by ID:
link_var(Remi) <- "ID"   # ON
link_type(Remi) <- "self"

link_var(Remi) <- NULL  # OFF

                          
## example 3: Wages data
data(wages)
wage <- qdata(wages[wages$id<2000,1:3])
print(qtime(exper,~lnw,wage,group=id))

wage$id <- as.factor(wage$id)
link_var(wage) <- "id"   # ON
link_type(wage) <- "self"
link_var(wage) <- NULL  # OFF

# example 4: Sunspots - for posterity
# Good to show off wrapping to investigate irregular series
data(sunspots)
head(sunspots)
str(sunspots)
qsun<-qdata(data.frame(Time=1:2820, sunspots))
qtime(Time, ~sunspots, qsun)

# example 4: lynx - for posterity
# Good to show off wrapping to investigate irregular series
data(lynx)
head(lynx)
str(lynx)
qlynx<-qdata(data.frame(Time=1:114, lynx))
qtime(Time, ~lynx, qlynx)
