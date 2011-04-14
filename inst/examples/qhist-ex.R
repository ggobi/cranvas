## examples of qhist()

require(cranvas)
require(ggplot2)
require(stringr)

data(tennis)
qtennis <- qdata(tennis)

qhist(qtennis, "First.Serve.Pct", horizontal = FALSE)

# Barret's testing examples
# torture
rows <- 1e+06
bigData <- qdata(data.frame(x = rnorm(rows), y = floor(rnorm(rows) * 7)))
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
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "stack", 
    title = "mtcars - stack")

# raw value items
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "identity", 
    title = "mtcars - identity")

# dodged items
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "dodge", 
    title = "mtcars - dodge")

# range from 0 to 1
qhist(mtcars, "disp", "cyl", horizontal = FALSE, stroke = "black", position = "relative", 
    title = "mtcars - relative")

####################
# interplay between different types of charts:

library(cranvas)
library(qtpaint)
library(plumbr)
library(productplots)


## color palette
library(RColorBrewer)

data(tennis)

qtennis = qdata(tennis)
qtennis$.brushed = FALSE
qtennis$.color = "grey30"

brush_attr(qtennis, ".brushed.size") <- 2
brush_attr(qtennis, ".brushed.color") <- "orange"

print(qhist(qtennis, "First.Serve.Pct", horizontal = FALSE))
print(qparallel(qtennis))
require(productplots)
print(qmosaic(qtennis, ~Country, "hbar")) 
