require(cranvas)

data(nrcstat)
nrcstat[,26]<- -nrcstat[,26]
colnames(nrcstat)[26]<-"Neg.Median.Time.to.Degree"
  
qnrc = qdata(nrcstat)

qscatter(qnrc, R.Rankings.5th.Percentile, R.Rankings.95th.Percentile)
