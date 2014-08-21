library(cranvas)

# Aspect ratio is calculated by optimizing banking to 45 degrees

## example 1: NASA temperature data
nasa2221 <- subset(nasa, Gridx == 22 & Gridy == 21)
nasa2221$Year <- factor(nasa2221$Year)
qnasa <- qdata(nasa2221)

# Small test that things work
# Use right/left arrow to wrap series
# Click g to change shift period
# Simple 1-1 linking between plots
qtime(TimeIndx,ts,qnasa,shift=c(1,12))
qscatter(ts,ps_tovs,data=qnasa)

# Connections between consecutive year separated, to help readability
qtime("TimeIndx","ts",qnasa,Year,shift=c(1,12))
# Shift-u/d to split multivariate time series
# R to make area plot
# Shift-y to wrap vertically, make a density plot
# m to change modes, so series can be selected
qtime(TimeIndx,c("ts","ca_med","ps_tovs"),qnasa,shift=c(1,12))
qtime("TimeIndx",c(ts,ca_med,ps_tovs),qnasa,Year)

##
nasa2221 <- subset(nasa, Gridx %in% c(14,17,20) & Gridy == 21)
#nasa2221 <- subset(nasa, Gridy == 21)
nasa2221$Year <- factor(nasa2221$Year)
nasa2221$Gridx <- factor(nasa2221$Gridx)
qnasa <- qdata(nasa2221)
# Example to show structural components, 
# Shift u/d for multivariate
# u/d for individual/location
qtime("TimeIndx",c(ts,ca_med,o3_tovs),qnasa,Gridx,shift=c(1,12))
qscatter(o3_tovs,ts,data=qnasa)
library(ggplot2)
nasa.locs <- subset(cranvas::nasa, TimeIndx == 1)
nasa.locs$loc <- paste(nasa.locs$Gridx, nasa.locs$Gridy, sep=",")
qplot(Long, Lat, data=nasa.locs, geom="text", label = loc)

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
library(dplyr)
wages.num <- summarise(group_by(wages, id), n=length(lnw))
indx <- wages.num$id[wages.num$n > 11]
wages.sub <- subset(wages, id %in% indx)
nindiv <- length(unique(wages.sub$id))
wages.sub$idno <- factor(wages.sub$id, labels=1:nindiv)
wages.sub.demog <- summarise(group_by(wages.sub, idno), n=length(lnw),
                       avlnw = mean(lnw, na.rm=T),
                       #trendlnw = lsfit(exper, lnw)$coef[2],
                       trendlnw = max(lnw, na.rm=T) - min(lnw, na.rm=T),
                       black = black[1],
                       hispanic = hispanic[1],
                       ged = ged[1], hgc = hgc[1],
                       avunemp = mean(uerate, na.rm=T))
#qwage <- qdata(wages[as.integer(as.character(wages$id))<2000,1:3])
qwages <- qdata(wages.sub[,c(11,2:3)])
qtime(exper, lnw, qwages, group=idno)
# id <- link_cat(wage, "id")
# remove_link(wage, id)

indx <- wages.num$id[wages.num$n > 3]
wages.sub2 <- subset(wages, id %in% indx)
wages.sub2.demog <- summarise(group_by(wages.sub2, id), n=length(lnw),
                             avlnw = mean(lnw, na.rm=T),
                             #trendlnw = coefficients(lm(lnw ~ exper))[2],
                             rangelnw = max(lnw, na.rm=T) - min(lnw, na.rm=T),
                             sdlnw = sd(lnw, na.rm=T),
                             startlnw = lnw[1],
                             endlnw = lnw[length(lnw)],
                             inclnw = (lnw[length(lnw)]-lnw[1])/lnw[1]*100.0,
                             black = black[1],
                             hispanic = hispanic[1],
                             ged = ged[1], hgc = hgc[1],
                             avunemp = mean(uerate, na.rm=T))
qwages <- qdata(wages.sub2[,1:3])
qwages.demog <- qdata(wages.sub2.demog)
id = link_cat(qwages.demog, "id", qwages, "id")
qtime(exper, lnw, qwages, group=id)
qscatter(startlnw, inclnw, qwages.demog)
qbar(hgc, qwages.demog)
qhist(avunemp, qwages.demog)
remove_link(qwages.demog, id[1])
remove_link(qwages, id[2])

## example 4: Lynx - for posterity
# Good to show off wrapping to investigate irregular series
# right/left arrow keys to shift series
qlynx <- qdata(data.frame(Time=1:114, lynx))
qtime(Time, lynx, qlynx, shift=1:13)

## example 5: Sunspots - for posterity
# Good to show off wrapping to investigate irregular series
# right/left arrow keys to shift series
qsun <- qdata(data.frame(Time=1:2820, sunspots))
qtime(Time, sunspots, qsun, shift=c(1,c(1,6,7,13,26)*10))

## example 6: Pigs
# Show of cross-correlations using shifting a single series
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
summary(flu.melt$Date)
flu.melt$Date[flu.melt$days>2500&flu.melt$days<2520]
# u/d to separate states
# left/right to wrap
qflu <- qdata(flu.melt)
qtime(days, FluSearches, data=qflu, group="State",shift=c(1,7,28,364))
# winter of 2014
flu2014 <- subset(flu.melt, days>3960)
ord <- names(sort(tapply(flu2014$FluSearches,flu2014$State,function(x)which(x>(max(x)/5*3))[1])))
flu2014$State <- factor(flu2014$State,levels=ord)
# u/d to separate states
qflu <- qdata(flu2014)
qtime(days, FluSearches, data=qflu, group="State",shift=c(1,7,28,35,91))

cranvas_off()
