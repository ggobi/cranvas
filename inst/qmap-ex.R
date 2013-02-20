library(cranvas)
library(maps)
library(ggplot2)

states <- map_data("state")
states$region <- factor(states$region)
qstates <- qdata(states)

qstates$.color <- sample(c("red", "blue"), nrow(states), replace=TRUE)
print(qmap(qstates, long, lat, group, region))

# recolor
qstates$.color <- sample(c("red", "blue"), nrow(states), replace=TRUE)

#######################

qcrimes <- qdata(crimes)
print(qparallel(data=qcrimes))

# now link map and crimes data set

link_cat(qcrimes, 'state', qstates, 'region')
print(qscatter(population, robbery, qcrimes))

# Choropleth maps
setMapColorByLabel(qstates, qcrimes, robbery/population, scale_colour_gradient())

# change to different choropleth map:
setMapColorByLabel(qstates, qcrimes, log(100000*robbery/population + 1), scale_colour_gradient2())
setMapColorByLabel(qstates, qcrimes, log(100000*burglary/population + 1), scale_colour_gradient2())

##############

source("load.R")

library(maps)
library(ggplot2)

counties <- map_data("county")
iowa <- subset(counties, region == "iowa")

qiowa <- qdata(iowa)

qmap(qiowa, long, lat, group, label = subregion)



source("load.R")
library(ggplot2)

qworld <- qdata(world)


qmap(qworld, long, lat, group, label = id)


library(profr)

#res <- profr(print(qmap(qworld, long, lat, group, label=id))) 


#################
# cartogram
# first example

data(cartdata)
# cartdata is a cartogram for the rate of burglaries from the crimes data set
# the cartogram algorithm is based on the one implemented in the cart package (R Forge)

# cartdata should have same length as qstates

cartdata$diffx <- cartdata$long - states$long
cartdata$diffy <- cartdata$lat - states$lat

# map to cartogram
for (i in 11:1) {
	x <- (i-1)/10
	qstates$long <- with(cartdata, long-x*diffx)
	qstates$lat <- with(cartdata, lat-x*diffy)
  scan()
}

# and back to the map again
for (i in 1:11) {
	x <- (i-1)/10
	qstates$long <- with(cartdata, long-x*diffx)
	qstates$lat <- with(cartdata, lat-x*diffy)
  scan()
}


