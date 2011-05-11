require(cranvas)

data(nrcstat)
nrcstat[,26]<- -nrcstat[,26]
colnames(nrcstat)[26]<-"Neg.Median.Time.to.Degree"
  
qnrc = qdata(nrcstat)

print(qscatter(qnrc, R.Rankings.5th.Percentile, R.Rankings.95th.Percentile))
print(qscatter(qnrc, S.Rankings.5th.Percentile, S.Rankings.95th.Percentile))
