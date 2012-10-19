setOldClass("QGraphicsScene")
setOldClass("QGraphicsView")
setOldClass("Qanviz::PlotView")
setOldClass("mutaframe")
setOldClass("mutalist")
setClassUnion("mutaframeORNULL", c("mutaframe","NULL"))
setClassUnion("QGraphicsSceneORNULL", c("QGraphicsScene","NULL"))
setClassUnion("Qanviz::PlotViewORNULL", c("Qanviz::PlotView","NULL"))

setRefClass("CranvasPlot", contains = "Cranvas",
            fields = list(
              data = "mutaframeORNULL",
              scene = "QGraphicsSceneORNULL",
              view = "Qanviz::PlotViewORNULL",
              layerList = "LayerListORNULL",
              meta = "CommonMeta"))


## constructor API
CranvasPlot <- function(..., data, scene, view, meta){
  lst <- list(...)
  if(length(lst) == 1 && is(lst[[1]], "LayerList"))
    layerList <- lst[[1]]
  else
    layerList <- LayerList(...)
  new("CranvasPlot", scene = scene, view = view, layerList = layerList, meta = meta, data = data)
}

## show method
setMethod("show", "CranvasPlot", function(object){
  object$view$show()
})

## print method
setMethod("print", "CranvasPlot", function(x){
  show(x)
})


