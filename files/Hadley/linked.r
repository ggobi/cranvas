library(plumbr)
library(qtpaint)

mtcarsm <- as.mutaframe(mtcars)
mtcarsm$.selected <- FALSE

draw_points <- function(data, x, y, selected = FALSE, colour = "black") {
  circle <- qglyphCircle(3)
  
  function(item, painter, exposed) {
    if (selected) {
      data <- data[data$.selected, ]
    }

    qfillColor(painter) <- colour
    qstrokeColor(painter) <- NA
    
    qdrawGlyph(painter, circle, data[[x]], data[[y]])  
  }
}

if (exists("view")) view$close()

scene <- qscene()
root <- qlayer(scene)
root$setLimits(qrect(range(mtcars$mpg), range(mtcars$wt)))
view <- qplotView(scene = scene)

points <- qlayer(root, draw_points(mtcarsm, "mpg", "wt"))
selected <- qlayer(root, draw_points(mtcarsm, "mpg", "wt", selected = T, 
  colour = "red"))
add_listener(mtcarsm, function(i, j) qupdate(selected))

print(view)
