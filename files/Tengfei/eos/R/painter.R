qglyphSector <- function(x,y,length,width,startAngle,sweepLength){
  len <- (length+width)*2
  r <- length+width
  x0 <- x-r
  y0 <- y-r
  glyph <- Qt$QPainterPath()
  movex <- x+r*cos(startAngle/180*pi)
  movey <- y+r*sin(startAngle/180*pi)
  glyph$moveTo(movex,movey)
  glyph$arcTo(x0,y0,len,len,-startAngle,-sweepLength)
  r <- r-width
  x0 <- x-r
  y0 <- y-r
  ## movex <- x+r*cos((startAngle+sweepLength)/180*pi)
  ## movey <- y+r*sin((startAngle+sweepLength)/180*pi)
  ##glyph$lineTo(movex,movey)
  glyph$arcTo(x0,y0,2*r,2*r,-(startAngle+sweepLength),sweepLength)
  glyph$closeSubpath()
  glyph
}

qglyphArc <- function(x,y,r,startAngle,sweepLength){
  x0 <- x-r
  y0 <- y-r
  len <- 2*r
  glyph <- Qt$QPainterPath()
  movex <- x+r*cos(startAngle/180*pi)
  movey <- y+r*sin(startAngle/180*pi)
  glyph$moveTo(movex,movey)
  glyph$arcTo(x0,y0,len,len,-startAngle,-sweepLength)
  glyph
}


## s <- qscene()
## paths <- qglyphArc(0,0,100,60,60)
## myfun <- function(layer,painter){
##    qdrawPath(painter,paths)  
## }
## l <- qlayer(s,myfun,limits=qrect(-140,-140,140,140))
## v <- qplotView(s)
## v$show()

