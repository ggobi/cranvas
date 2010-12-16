library(qtpaint)
scene = qscene()
figLayer = qlayer(scene,clip=FALSE)
titlePainter = function(item, painter) {
    qdrawText(painter, paste(rep('The Useless Title', 5), collapse=' - '), 300, 10,'center','center')
}
yAxisPainter = function(item, painter) {
    qdrawText(painter, as.character(pretty(1:400)), 37.5, pretty(1:400))
}
plotPainter = function(item, painter) {
    qdrawRect(painter, -100+1, -100+1, 700-1, 700-1,fill='gray80')
    qdrawPoint(painter, runif(1000, 0, 600), runif(1000, 0, 400))
}
xAxisPainter = function(item, painter) {
    qdrawText(painter, as.character(pretty(1:600)), pretty(1:600), 37.5)
}
mouseLocator = function(item, event){
    print(as.numeric(event$pos()))
}
# titleLayer = qlayer(figLayer, titlePainter, mousePressFun=mouseLocator, limits=qrect(0,0,600,20), clip=FALSE,row=0,col=1)
# yaxis = qlayer(figLayer, yAxisPainter,mousePressFun=mouseLocator, limits=qrect(0,0,75,400),clip=FALSE,row = 1,col=0)
# plotLayer = qlayer(figLayer, plotPainter, mousePressFun=mouseLocator, limits=qrect(0,0,600,400),clip=FALSE,row = 1, col = 1)
# xaxis = qlayer(figLayer, xAxisPainter, mousePressFun=mouseLocator, limits=qrect(0,0,600,75),clip=FALSE,row = 2, col = 1)

titleLayer = qlayer(figLayer, titlePainter, mousePressFun = mouseLocator,
   limits = qrect(0, 0, 700, 20), row = 0, col = 1)

yaxis = qlayer(figLayer, yAxisPainter, mousePressFun = mouseLocator,
   limits = qrect(-10,-10, 75, 500), row = 1, col = 0)

plotLayer = qlayer(figLayer, plotPainter, mousePressFun = mouseLocator,
   limits = qrect(-100, -100, 700, 500), row = 1, col = 1)

xaxis = qlayer(figLayer, xAxisPainter, mousePressFun = mouseLocator,
   limits = qrect(-10, -10, 700, 95), row = 2, col = 1)

# layers nested in layers (more convenient for faceting?)
# qlayer(plotLayer, plotPainter, limits=qrect(0,0,600,400),row = 0, col = 0)
# qlayer(plotLayer, plotPainter, limits=qrect(0,0,600,400),row = 1, col = 1)

layout = figLayer$gridLayout()

layout$setRowStretchFactor(0, 1)
layout$setRowStretchFactor(1, 5)
layout$setRowStretchFactor(2, 1)
layout$setColumnStretchFactor(0, 1)
layout$setColumnStretchFactor(1, 5)

print(view<-qplotView(scene = scene))

