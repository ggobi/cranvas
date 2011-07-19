library(cranvas)
library(productplots)

data(Titanic)
titanic <- as.data.frame(Titanic)
qtitanic <- qdata(titanic)

print(qmosaic(qtitanic, Freq ~ Survived, "hbar")) 
print(qmosaic(qtitanic, Freq ~ Age, "hbar")) 
print(qmosaic(qtitanic, Freq ~ Sex, "hbar")) 
print(qmosaic(qtitanic, Freq ~ Class, "hbar")) 


print(qmosaic(qtitanic, Freq ~ Sex + Age | Class, mosaic()))
