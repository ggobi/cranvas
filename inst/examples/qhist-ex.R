## examples of qhist()

library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)
library(ggplot2)

data(tennis)
qtennis <- qmutaframe(tennis)

qhist(qtennis, "First.Serve.Pct", horizontal=FALSE)

# Barret's testing examples
# torture
rows <- 1000000
bigData <- qmutaframe(data.frame(x = rnorm(rows), y = floor(rnorm(rows) * 7)))
qhist(bigData)

# each column is split evenly
qhist(bigData, splitByCol = "y", title = "Torture - stack") 
qhist(bigData, splitByCol = "y", title = "Torture - stack", horizontal = FALSE) 

# each column has similar height colors
qhist(bigData, splitByCol = "y", title = "Torture - dodge", position = "dodge") 

# range from 0 to 1
qhist(bigData, splitByCol = "y", title = "Torture - relative", position = "relative") 


# color tests
# all color is defined
qhist(mtcars, "disp", horizontal = TRUE, fill = "gold", stroke = "red4")

# stacked items
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "stack", title = "mtcars - stack")

# raw value items
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "identity", title = "mtcars - identity")

# dodged items
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "dodge", title = "mtcars - dodge")

# range from 0 to 1
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "relative", title = "mtcars - relative")
