library(cranvas)
data(wages)

\dontrun{
## this is a very simple linear regression assuming each man has the same slope, which may not be entirely reasonable
fit = lm(lnw~exper+id,data=wages)
wage2=subset(wages,!duplicated(wages$id), c(id, hispanic))
## predict wages at year 0 and 6 respectively
wage2$lnw0 = predict(fit, data.frame(id=unique(wages$id), exper=0))
wage2$lnw6 = predict(fit, data.frame(id=unique(wages$id), exper=6))
qwage2=qdata(wage2,color=hispanic)

qhist(lnw0, main = 'Wages on the first day')
qhist(lnw6, main = 'Wages after 6 years')
}
