library(qtbase)
library(qtpaint)
library(cranvas)


library(maps)
library(ggplot2)

states <- map_data("state")
qstates <- qdata(states)

#qtmap(qstates, long, lat, group)


#######################

crimes <- read.csv("http://www.hofroe.net/stat579/crimes-09.csv")
crimes$State <- tolower(crimes$State)

## color palette
library(RColorBrewer)
crimes$nines <- with(crimes, cut(Burglary/Population, 9))
#crimes$nines <- with(crimes, cut(Violent.crime/Population, 9))
crimes$nines <- brewer.pal(9, "Greys")[crimes$nines]
qcrimes <- qdata(crimes)
print(qparallel(qcrimes))

source("map.R")
print(qtmap(qstates, long, lat, group, label = region, labeldata = qcrimes, 
    by.x = "region", by.y = "State", colour = Violent.crime))
#qtmap(qstates, long, lat, group, label=region, labeldata=qcrimes,
#   by.x='region', by.y='State', colour=I(Violent.crime/Population))


#qtmap(qstates, long, lat, group, label=region)
#qtmap(qstates, long, lat, group)




#world <- map_data('world')
#qworld <- qdata(world)

#qtmap(qworld, long, lat, group) 
