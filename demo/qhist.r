# demo for qhist
# require(cranvas)

# toture
rows <- 1000000
bigData <- qmutaframe(data.frame(x = rnorm(rows), y = floor(rnorm(rows) * 7)))
qhist(bigData)

# each column is split evenly
qhist(bigData, splitByCol = "y", title = "Toture - stack") 
qhist(bigData, splitByCol = "y", title = "Toture - stack", horizontal = FALSE) 

# each column has similar height colors
qhist(bigData, splitByCol = "y", title = "Toture - dodge", position = "dodge") 

# range from 0 to 1
qhist(bigData, splitByCol = "y", title = "Toture - relative", position = "relative") 


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