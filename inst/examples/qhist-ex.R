## examples of qhist()

require(ggplot2)
#require(stringr)

#require(qtbase)
#require(qtpaint)
#require(plumbr)
require(cranvas)

data(tennis)
qtennis <- qdata(tennis)
qhist("First.Serve.Pct", qtennis, horizontal = FALSE)
qhist("Serve.Speed", qtennis, horizontal = FALSE)

# Barret's testing examples
# torture
rows <- 1e+04
bigData <- qdata(data.frame(x = rnorm(rows), y = floor(rnorm(rows) * 7)))
qhist(x, bigData)
qscatter(x,y, data= bigData)

# each column is split evenly
qhist(x, bigData, splitByCol = "y", title = "Torture - stack")
qhist(x, bigData, splitByCol = "y", title = "Torture - stack", horizontal = FALSE)

# each column has similar height colors
qhistx, (bigData, splitByCol = "y", title = "Torture - dodge", position = "dodge")

# range from 0 to 1
qhist(x, bigData, splitByCol = "y", title = "Torture - relative", position = "relative")


# color tests
# all color is defined
qhist("disp", mtcars, horizontal = TRUE, fill = "gold", stroke = "red4")

# stacked items
qhist("disp", mtcars, "cyl", horizontal = FALSE, stroke = "black", position = "stack",     title = "mtcars - stack")

# raw value items
qhist("disp", mtcars, "cyl", horizontal = FALSE, stroke = "black", position = "identity", 
    title = "mtcars - identity")

# dodged items
qhist("disp", mtcars, "cyl", horizontal = FALSE, stroke = "black", position = "dodge", 
    title = "mtcars - dodge")

# range from 0 to 1
qhist("disp", mtcars, "cyl", horizontal = FALSE, stroke = "black", position = "relative", 
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

print(qhist("First.Serve.Pct", qtennis, horizontal = FALSE))
print(qparallel(qtennis))
require(productplots)
print(qmosaic(qtennis, ~Country, "hbar")) 
