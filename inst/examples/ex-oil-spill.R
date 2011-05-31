library(qtbase)
library(qtpaint)
library(plumbr)
require(stringr)
library(cranvas)
library(ggplot2)
library(productplots)

# Oil Spill Data

# floats data
data(floats)
floats$call <- as.numeric(floats$callSign)
floats <- na.omit(floats)
floats <- subset(floats, (callSign != "Q4901044"))
floats$callSign <- factor(floats$callSign)

# prep floats
floats$date <- as.Date(floats$Date_Time, format="%Y-%m-%d")
floats$day <- as.numeric(floats$date - as.Date("2010-04-20", format="%Y-%m-%d"))
floats$week <- (floats$day %/% 7) + 1



qfl <- qdata(floats)
qfl$.color <- "grey20"
#brush(qfl, "color") <- "yellow"
brush(qfl, "label.color") <- "grey30"
qhist(data=qfl, xCol="Depth", binwidth=10, horizontal=FALSE)
qscatter(Longitude, Latitude, data=qfl)

#
qmosaic(data=qfl, ~callSign, "vbar")
#
qhist(data=qfl, xCol="day", binwidth=1, horizontal=FALSE)
qhist(data=qfl, xCol="day", binwidth=7, horizontal=FALSE)
#
qscatter(Temperature, Salinity, data=qfl)

qparallel(data=qfl, vars=c(8,10,12), alpha=0.1)



