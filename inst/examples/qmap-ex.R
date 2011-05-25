library(qtbase)
library(qtpaint)
library(cranvas)
library(plumbr) 
library(maps)
library(ggplot2)

states <- map_data("state")
qstates <- qdata(states)

qstates$.color <- sample(c("red", "blue"), nrow(states), replace=TRUE)
qmap(qstates, long, lat, group, region)

# recolor
qstates$.color <- sample(c("red", "blue"), nrow(states), replace=TRUE)


#######################

data(crimes)
crimes$State <- tolower(crimes$State)

qcrimes <- qdata(crimes)
print(qparallel(data=qcrimes))

# now link map and crimes data set
link_var(qcrimes) = "State"
link_var(qstates) = "region"

link(qcrimes, qstates)
qscatter(qcrimes, Population, Robbery)




# Choropleth maps
setMapColorByLabel(qstates, qcrimes, Robbery/Population, scale_colour_gradient())

# change to different choropleth map:
setMapColorByLabel(qstates, qcrimes, log(100000*Robbery/Population + 1), scale_colour_gradient2())

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

data(world)
qworld <- qdata(world)


qmap(qworld, long, lat, group, label = id)


library(profr)

#res <- profr(print(qmap(qworld, long, lat, group, label=id))) 
