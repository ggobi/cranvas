setOldClass("Qanviz::RLayer")
setClassUnion("Qanviz::RLayerORNULL", c("Qanviz::RLayer","NULL"))
## S4 class for a list of Layer and with validation.
## I name it to Layer to make it easy to rember althouth, it's RLayer.
setClass("LayerList", contains = "list")
setClassUnion("LayerListORNULL", c("LayerList","NULL"))
## validation
setValidity("LayerList", function(object){
  res <- unlist(lapply(object, is, "Qanviz::RLayer"))
  if(all(res))
    TRUE
  else
    paste("Entry ", which(!res), " is not Qanviz::RLayer object(returned by qlayer)")
})

## LayerList API 
LayerList <- function(...){
  lst <- list(...)
  if(length(lst) == 1 && is.list(lst[[1]]))
    lst <- lst[[1]]
  ## validation
  isLayer <- unlist(lapply(lst, is, "Qanviz::RLayer"))
  if(!all(isLayer))
    stop(paste("Entry ", which(!isLayer), " is not Qanviz::RLayer object(returned by qlayer)"))
  new("LayerList", lst)
}


## l1 = qlayer()
## l2 = qlayer()
## LayerList(l1, l2)
## LayerList(list(l1, l2))
## l3 = "nothing"
## LayerList(l1, l2, l3)
