setwd('~/Prolang/git/cranvas/Tengfei/eos/R')
source('qcircle-utils.R')
source('qcircle-painter.R')
options(stringsAsFactors=FALSE)

## should write a qcircle to plot categorical data


## draw a curve
library(qtpaint)
library(plumbr)
s <- qscene()
paths <- list(qpathQuadCurve(c(-120,-120),c(0,100),c(140,140)))
myfun <- function(layer,painter){
  ##    qantialias(painter) <- FALSE
  qlineWidth(painter) <- 1
  qdrawPath(painter,paths,stroke=rainbow(length(paths)))  
}
l <- qlayer(s,myfun,limits=qrect(-140,-140,140,140),
            geometry=qrect(0,0,600,600))
v <- qplotView(s)
v$show()

## draw a list of curves
library(qtpaint)
s <- qscene()
theta1 <- runif(100,0,2*pi)
theta2 <- runif(100,0,2*pi)
r <- 100
point1 <- cbind(r*cos(theta1),r*sin(theta1))
point2 <- cbind(r*cos(theta2),r*sin(theta2))
paths <- lapply(1:length(theta1),function(i){
  qpathQuadCurve(as.numeric(point1[i,]),c(0,0),as.numeric(point2[i,]))
})
mouseHover <- function(layer,event){
  pos <<- event$pos()
  idx <- which(sapply(paths,function(x){
    x$contains(pos)
  }))
  if(length(idx)>0){
  cols[idx,1] <- "purple"
}
}

cols <- mutaframe(rep("gray80",length(paths)))

add_listener(cols,function(i,j){
  print(i)
  print(j)
  qupdate(s)
})

myfun <- function(layer,painter){
  ##    qantialias(painter) <- FALSE
  qlineWidth(painter) <- 1
  ##    cols <- as.character(as.data.frame(cols))
  qdrawPath(painter,paths,stroke=as.data.frame(cols)[,1])
}

l <- qlayer(s,myfun,limits=qrect(-140,-140,140,140),hoverMoveFun=mouseHover,
            geometry=qrect(0,0,600,600))
v <- qplotView(s)
v$show()





## Try to create a bundle
theta1 <- seq(0,pi/2,length=10)
theta2 <- rev(seq(3*pi/4,6*pi/4,length=10))
theta3 <- theta1+pi/50
theta4 <- theta2+pi/50
r <- 100
point1 <- cbind(r*cos(theta1),r*sin(theta1))
point2 <- cbind(r*cos(theta2),r*sin(theta2))
point3 <- cbind(r*cos(theta3),r*sin(theta3))
point4 <- cbind(r*cos(theta4),r*sin(theta4))
s <- qscene()
## paths <- list(qpathCurveBundle(c(-120,-120),c(-85,60),c(-120,140),
##                                 c(-50,140),c(-85,60),c(-50,-120)))
## paths <- list(qpathCurveBundle(c(-50,-120),c(-85,60),c(-120,140),
##                                 c(-50,140),c(-85,60),c(-120,-120)))
paths <- lapply(1:length(theta1),function(i){
  qpathCurveBundle(as.numeric(point1[i,]),
                   c(0,0),
                   as.numeric(point2[i,]),
                   as.numeric(point4[i,]),
                   c(0,0),
                   as.numeric(point3[i,]))
})

myfun <- function(layer,painter){
  ##    qantialias(painter) <- FALSE
  qfillColor(painter) <- rainbow(length(paths))
  ##    qlineWidth(painter) <- 2
  qdrawPath(painter,paths)
}
l <- qlayer(s,myfun,limits=qrect(-140,-140,140,140),
            geometry=qrect(0,0,600,600))
v <- qplotView(s)
v$show()

## try to draw some sectors
s <- qscene()
sectorLength <- 100
sectorWidth <- 20
sectorStart <- seq(0,2*180,length=10)
sectorSweep <- 30
paths <- lapply(1:10,function(i){
  qpathSector(0,0,sectorLength,sectorWidth,sectorStart[i],sectorSweep)
})
pfun <- function(layer,painter){
  qdrawPath(painter,paths)
}
l <- qlayer(s,myfun,limits=qrect(-140,-140,140,140),
            geometry=qrect(0,0,600,600))
v <- qplotView(s)
v$show()


## try some categorical data, make a general tools
## tdf <- as.data.frame(Titanic)
## head(tdf)
## library(reshape)
## cls <- ddply(tdf,.(Class),summarise,Sum=sum(Freq))
## cls2 <- ddply(tdf,.(Class,Sex),summarise,Sum=sum(Freq))
## cls3 <- ddply(tdf,.(Class,Sex,Age),summarise,sum=sum(Freq))
## sur <- ddply(tdf,.(Survived),summarise,sum=sum(Freq))
  
