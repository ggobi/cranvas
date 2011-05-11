# Barret's testing examples
# torture
require(ggplot2)
require(cranvas)

rows <- 1e+04
bigData <- qdata(data.frame(x = rnorm(rows), y = floor(rnorm(rows) * 7)))
print(qhist(x, bigData))
print(qscatter(x,y, data= bigData))

# each column is split evenly
print(qhist(x, bigData, splitByCol = "y", title = "Torture - stack"))
print(qhist(x, bigData, splitByCol = "y", title = "Torture - stack", horizontal = FALSE))

# each column has similar height colors
print(qhistx, (bigData, splitByCol = "y", title = "Torture - dodge", position = "dodge"))

# range from 0 to 1
print(qhist(x, bigData, splitByCol = "y", title = "Torture - relative", position = "relative"))

# color tests
# all color is defined
print(qhist("disp", mtcars, horizontal = TRUE, fill = "gold", stroke = "red4"))

# stacked items
print(qhist("disp", mtcars, "cyl", horizontal = FALSE, stroke = "black", position = "stack",     title = "mtcars - stack"))

# raw value items
print(qhist("disp", mtcars, "cyl", horizontal = FALSE, stroke = "black", position = "identity", title = "mtcars - identity"))

# dodged items
print(qhist("disp", mtcars, "cyl", horizontal = FALSE, stroke = "black", position = "dodge", title = "mtcars - dodge"))

# range from 0 to 1
print(qhist("disp", mtcars, "cyl", horizontal = FALSE, stroke = "black", position = "relative", title = "mtcars - relative"))

