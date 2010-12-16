library(plumbr)
library(qtpaint)
args(qlayer)
mydf <- mutaframe(x=rnorm(100,50,20),
                   y=rnorm(100,50,20),
                  color=I('black'))

## two scatter plots connects to this mutaframe
mouseDB <- function(layer,event){
  rect <- qrect(0,0,10,10)
  mat <- layer$deviceTransform(event)$inverted()
  rect <- mat$mapRect(rect)
  pos <- event$pos()
  rect$moveCenter(pos)
  idx <- layer$locate(rect)+1
  mydf$color[idx] <- I('red')
}
p1 <- function(layer,painter){
  qdrawCircle(painter,mydf$x,mydf$y,r=5,fill=mydf$color)
}
p2 <- function(layer,painter){
    qdrawCircle(painter,mydf$x,mydf$y,r=10,fill=mydf$color)
}

s1 <- qscene()
s2 <- qscene()

l1 <- qlayer(s1,p1,mouseDoubleClickFun=mouseDB,limits=qrect(0,0,100,100))
l2 <- qlayer(s2,p2,mouseDoubleClickFun=mouseDB,limits=qrect(0,0,100,100))
v1 <- qplotView(s1)
v1$show()
v2 <- qplotView(s2)
v2$show()

## add listener to this mutaframe
add_listener(mydf,function(i,j){
  qupdate(l1)
  qupdate(l2)
})



## Then when you change the color slots
## mydf$color[70] <- I('black')
## mydf$color[60] <- I('blue')
## change back
## mydf$color <- I('red')

## both plots update
