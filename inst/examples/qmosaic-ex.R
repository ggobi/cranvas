library(cranvas)
library(productplots)

data(happy)
qhappy <- qdata(happy, color=happy)
qhappy <- qdata(happy, color=sex)
qhappy <- qdata(happy)

qmosaic(qhappy, ~marital+happy, divider=c("vspine", "hspine"))
qmosaic(qhappy, ~marital+happy+year, divider=c("vspine", "hspine", "hspine"))

qbar(sex, qhappy)

data(Titanic)
titanic <- as.data.frame(Titanic)
qtitanic <- qdata(titanic)

qtitanic$.brushed <- qtitanic$Survived=="Yes"
print(qmosaic(qtitanic, Freq ~ Sex + Age + Class))
print(qbar(Survived, data=qtitanic))

print(qmosaic(qtitanic, Freq ~ Survived, "hbar")) 
print(qmosaic(qtitanic, Freq ~ Age, "hbar")) 
print(qmosaic(qtitanic, Freq ~ Sex, "hbar")) 
print(qmosaic(qtitanic, Freq ~ Class, "hbar")) 

print(qmosaic(qtitanic, Freq ~ Sex + Survived | Class, divider=c("vspine", "hspine", "hspine")))
data(happy)
qhappy <- qdata(happy)
qmosaic(qhappy, ~ happy, "hbar")
qmosaic(qhappy, ~ happy + age, c("hspine", "vspine"))
