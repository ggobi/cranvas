library(cranvas)

## old iris as the toy example
qiris = qdata(iris)
qparallel(~., data = qiris)
record_selector(Species, data = qiris)

## NRC rankings
data(nrcstat)
qnrc = qdata(nrcstat)
qparallel(10:13, data = qnrc, main = 'Overview of Rankings', horizontal=FALSE)
record_selector(Institution, data = qnrc)
qparallel(14:19, data = qnrc, main = 'Research, Student Support, Diversity')
qparallel(20:26, data = qnrc, main = 'Publication, Award, Time to Degree')
