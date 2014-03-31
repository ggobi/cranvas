library(cranvas)

## example 1: NASA temperature data
nasa2221 <- subset(nasa, Gridx == 22 & Gridy == 21)
nasa2221$Year <- factor(nasa2221$Year)
qnasa <- qdata(nasa2221)

qtime(TimeIndx,ts,qnasa,shift=c(1,12))
qscatter(ts,ps_tovs,data=qnasa)

qtime("TimeIndx","ts",qnasa,Year,shift=1)
qtime(TimeIndx,c("ts","ps_tovs","ca_med"),qnasa,shift=c(1,12))
qtime("TimeIndx",c(ts,ps_tovs,ca_med),qnasa,period=Year)


## example 2: Remifentanil in the nlme package
library(nlme)
qRem <- qdata(Remifentanil[complete.cases(Remifentanil) & Remifentanil$ID==1,])
qtime(Time, conc, qRem)

Remi <- Remifentanil[complete.cases(Remifentanil),]
Remi$ID <- factor(Remi$ID)
qRemi <- qdata(Remi)
qtime(Time, conc, qRemi, group=ID)
qscatter(Amt, conc, data=qRemi)
# for categorical brushing self-link dataset by ID:
# id <- link_cat(qRemi, "ID")
# remove_link(qRemi, id)


## example 3: Wages
qwage <- qdata(wages[as.integer(as.character(wages$id))<2000,1:3])
qtime(exper, lnw, qwage, group=id)
# id <- link_cat(wage, "id")
# remove_link(wage, id)


## example 4: Lynx - for posterity
# Good to show off wrapping to investigate irregular series
qlynx <- qdata(data.frame(Time=1:114, lynx))
qtime(Time, lynx, qlynx, shift=1:12)


## example 5: Sunspots - for posterity
# Good to show off wrapping to investigate irregular series
qsun <- qdata(data.frame(Time=1:2820, sunspots))
qtime(Time, sunspots, qsun, shift=c(1,(1:10)*10))


## example 6: Pigs
qpig <- qdata(pigs)
qtime(TIME, c("GILTS","PROFIT","PRODUCTION","HERDSZ"), qpig, shift=c(1,4))


## example 7: flu trends
flu.data <- read.table("http://www.google.org/flutrends/us/data.txt", skip=11, sep=",", header=TRUE)
# Get only states
flu.data <- flu.data[, c(1, 3:53)]
# Melt data, and rename variables
library(reshape)
flu.melt <- melt(flu.data, id.vars="Date")
flu.melt$Date <- as.Date(flu.melt$Date)
colnames(flu.melt)[2] <- "State"
colnames(flu.melt)[3] <- "FluSearches"
flu.melt$days <- as.vector(difftime(flu.melt$Date,as.Date('2002-12-31')))
# winter of 2014
flu2014 <- subset(flu.melt, days>3960)
ord <- names(sort(tapply(flu2014$FluSearches,flu2014$State,function(x)which(x>(max(x)/5*3))[1])))
flu2014$State <- factor(flu2014$State,levels=ord)
qflu <- qdata(flu2014)
qtime(days, FluSearches, data=qflu, group="State")

cranvas_off()
