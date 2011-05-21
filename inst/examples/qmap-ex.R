source("load.R")

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
values <- with(crimes, Robbery/Population)
cs <- scale_colour_gradient(limits=range(values))
temp <- data.frame(State=crimes$State, colors=cs$map(values))
qstates$.color <- as.character(merge(states, temp, by.x="region", by.y="State", all.x=T)$colors)

# change to different choropleth map:
values <- with(crimes, log(100000*Robbery/Population + 1))
cs <- scale_colour_gradient2(limits=range(values))
temp <- data.frame(State=crimes$State, colors=cs$map(values))
qstates$.color <- as.character(merge(states, temp, by.x="region", by.y="State", all.x=T)$colors)



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
