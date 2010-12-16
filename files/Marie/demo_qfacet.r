source("qfacet.r")

require(RColorBrewer)
require(plumbr)


mtcars.col <- brewer.pal(8, "Set1")[as.integer(mtcars$cyl)]
qmtcars <- qmutaframe(mtcars, .brushed = FALSE, .color = mtcars.col)

set_brush_attr(qmtcars, '.brushed.size', 2)
set_brush_attr(qmtcars, '.brushed.color', "orange")

#### inspiration: ggplot graphic ##
#require(ggplot2)
#mtcarsdf <- as.data.frame(qmtcars)
#print(ggplot(data = mtcarsdf, aes(x = hp, y = wt)) + geom_point()+ facet_grid(cyl~gear))

### demo1: standalone ###
display <- qfacet(data = qmtcars, facetformula = cyl~gear, xyformula = wt~hp)
#print(display)

### demo2: linked ###
source("../cranvas/files/Heike/mosaic-hilite.r", chdir = T)
print(qmosaic(qmtcars, ~gear,"hbar"))

