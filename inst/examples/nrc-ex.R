## Looking at the NRC rankings data with cranvas

library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)

# scatterplots of ratings variables, 5th vs 95th percentiles
# parallel coords of criteria
# lookup text window
data(nrcstat)
qnrc = qmutaframe(nrcstat)
rownames(qnrc) = paste(nrcstat$Institution.Name, nrcstat$Program.Name, sep = ' -> ')
qnrc$.color = 'red'

library(RGtk2)
library(gWidgets)
data_selector(qnrc, "Institution.Name", "RGtk2")

qscatter(qnrc, R.Rankings.5th.Percentile~R.Rankings.95th.Percentile)
qscatter(qnrc, S.Rankings.5th.Percentile~S.Rankings.95th.Percentile)
         
median.centering<-function(x) {
  x<-(x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T))
  x<-x-median(x, na.rm=T)
}
nrc.medctr<-apply(nrcstat[,20:68], 2, median.centering)
var.ord<-order(nrc.medctr[13,])+19
qparallel(qnrc, vars=var.ord, main='Other Indicators', center=median, horizontal=FALSE, glyph='tick', lab.split=NULL, boxplot=TRUE, boxwex=.8)



