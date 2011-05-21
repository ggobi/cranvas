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

## color palette   
library(RColorBrewer)
qcrimes <- qdata(crimes)
print(qparallel(data=qcrimes))

# now link map and crimes data set
link_var(qcrimes) = "State"
link_var(qstates) = "region"

link(qcrimes, qstates)
qscatter(qcrimes, Population, Robbery)


# the examples below don't work at the moment
#print(qmap(qstates, long, lat, group, label=region, labeldata=qcrimes,
#   by.x='region', by.y='State', colour=Violent.crime))
q1 <- qmap(qstates, long, lat, group, label = region, labeldata = qcrimes, 
    by.x = "region", by.y = "State", colour = log(Violent.crime/Population + 1))
q1
q2 <- qmap(qstates, long, lat, group, label = region, labeldata = qcrimes, 
    by.x = "region", by.y = "State", colour = log(Motor.vehicle.theft/Population + 1))
q2







#qmap(qstates, long, lat, group, label=region)
#qmap(qstates, long, lat, group)


##############

library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)


library(maps)
library(ggplot2)

counties <- map_data("county")
iowa <- subset(counties, region == "iowa")

qiowa <- qdata(iowa)

qmap(qiowa, long, lat, group, label = subregion)



source("load.R")
library(ggplot2)

#world <- map_data('world')
data(world)
world$group <- as.numeric(as.factor(world$group))
qworld <- qdata(world)


qmap(qworld, long, lat, group, label = id)


library(profr)

#res <- profr(print(qmap(qworld, long, lat, group, label=id))) 
