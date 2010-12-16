# source("~/Documents/cranvas/demos/random-walk.r")
library(qtpaint)
library(qtbase)
library(ggplot2)

n <- 50000
x <- runif(n)
y <- runif(n)
df <- data.frame(X = x, Y = y)

scatterplot <- function(item, painter, exposed) {
  # print(min(view_size(item)) / 100)
  circle <- qglyphCircle(2)
  qfillColor(painter) <- alpha("red", 1/20)
  qstrokeColor(painter) <- NA
  qdrawGlyph(painter, circle, df[,1], df[,2])
}

scene <- qscene()
root <- qlayer(scene)

points <- qlayer(root, scatterplot, limits=qrect(range(df[,1]), range(df[,2])))

view <- qplotView(scene = scene)
print(view)


timer <- qtimer(1/30, function() {
  df$X <<- df$X + runif(nrow(df), -0.01, 0.01)
  qupdate(scene)  
})
timer$start()
# timer$stop()
