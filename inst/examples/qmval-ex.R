library(cranvas)

## BRFSS data
data(brfss)

qbrfss <- qdata(brfss)
qmval(names(brfss)[40:50])
qmval(51:68)
qmval(~POORHLTH+FRUIT+GREENSAL)

qparallel(100:110)

## TAO data
data(tao, package = 'tourr')

qtao <- qdata(tao)
qmval(~.)
qmval(~., horizontal = FALSE, standardize = FALSE, main = 'horizontal plot with counts')
qscatter(longitude, latitude)
