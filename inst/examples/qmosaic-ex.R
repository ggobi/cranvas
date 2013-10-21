library(cranvas)
library(productplots)
library(plyr)


## Simpson on board the Titanic?!
data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Passenger <- titanic$Class != "Crew"
qtitanic <- qdata(titanic, color=Survived)

qtitanic$.brushed <- qtitanic$Survived=="Yes"
## passengers have a higher probability of survival than crew members ...
qmosaic(qtitanic, Freq ~ Passenger, divider=c("hspine"))
## unless sex is taken into account: the conditional association between Passenger
## and Survival changes to the opposite: both male and female crew members had better
## changes of survival than their counterparts on the passenger side.
qmosaic(qtitanic, Freq ~ Passenger|Sex, divider=c("hspine", "hspine"))

## this is an example of the Simpson's paradox, and is due to the strong association
## between crew membership and gender.
qmosaic(qtitanic, Freq ~ Sex+Passenger, divider=c("vspine", "hspine"))

#######################
data(happy)
qhappy <- qdata(happy, color=happy)

qmosaic(qhappy, ~sex+marital, divider=c("vspine", "hspine"))
qmosaic(qhappy, ~marital+happy+year, divider=c("vspine", "hspine", "hspine"))
qmosaic(qhappy, ~marital+degree, divider=c("vspine", "hspine"))
qmosaic(qhappy, ~happy, divider=c("hbar"))

qbar(sex, qhappy)


qmosaic(qtitanic, Freq ~ Sex + Age + Class)
qbar(Survived, data=qtitanic)

qmosaic(qtitanic, Freq ~ Survived, "hbar")
qmosaic(qtitanic, Freq ~ Age, "hbar")
qmosaic(qtitanic, Freq ~ Sex, "hbar")
qmosaic(qtitanic, Freq ~ Class, "hbar")

qmosaic(qtitanic, Freq ~ Sex + Survived | Class, divider=c("vspine", "hspine", "hspine"))
data(happy)
qhappy <- qdata(happy)
qmosaic(qhappy, ~ happy, "hbar")
qmosaic(qhappy, ~ happy + age, c("hspine", "vspine"))

cranvas_off()
