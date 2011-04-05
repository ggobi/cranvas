require(qtbase)
require(qtpaint)
require(cranvas)


require(stringr)
require(productplots)
require(plumbr)

data(Titanic)
titanic <- as.data.frame(Titanic)
qtitanic <- qdata(titanic)

print(qmosaic(qtitanic, Freq ~ Sex + Age | Class, mosaic()))
print(qmosaic(qtitanic, Freq ~ Survived, "hbar")) 
