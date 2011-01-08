require(qtbase)
require(qtpaint)
require(cranvas)


require(stringr)
require(productplots)
require(plumbr)

data(Titanic)
titanic <- as.data.frame(Titanic)

tit.col = c("grey30","grey50")[as.integer(titanic$Survived)]

qtitanic <- qmutaframe(titanic, .brushed = FALSE, .color=tit.col)
brush_attr(qtitanic, '.brushed.color') <- "red"

print(qmosaic(qtitanic, Freq~Sex +Age|Class, mosaic()))
print(qmosaic(qtitanic, Freq~Survived, "hbar"))
