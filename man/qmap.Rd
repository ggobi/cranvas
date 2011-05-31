\name{qmap}
\alias{qmap}
\title{Interactive Maps}
\usage{qmap(data, longitude, latitude, group, label = group, 
    main, ...)

}
\description{
Create an interactive map from qdata}
\value{NULL}
\author{Heike Hofmann}
\arguments{\item{data}{a mutaframe which is typically built upon a data frame
along with several row attributes}
\item{latitude}{spatial x variable}
\item{longitude}{spatial y variable}
\item{group}{grouping variable for polygons}
\item{main}{window title, by default 'Map of <data set>'}
\item{...}{}
}
\examples{library(qtbase)
library(qtpaint)
library(cranvas)
library(plumbr)

library(maps)
library(ggplot2)

states <- map_data("state")
qstates <- qdata(states)

qtmap(qstates, long, lat, group)


#######################

data(crimes)
crimes$State <- tolower(crimes$State)

## color palette
library(RColorBrewer)
qcrimes <- qdata(crimes)
print(qparallel(data = qcrimes))

#print(qtmap(qstates, long, lat, group, label=region,
#   labeldata=qcrimes,
# by.x='region', by.y='State', colour=Violent.crime))
q1 <- qtmap(qstates, long, lat, group, label = region, 
    labeldata = qcrimes, by.x = "region", by.y = "State", 
    colour = log(Violent.crime/Population + 1))
q1
q2 <- qtmap(qstates, long, lat, group, label = region, 
    labeldata = qcrimes, by.x = "region", by.y = "State", 
    colour = log(Motor.vehicle.theft/Population + 1))
q2







#qtmap(qstates, long, lat, group, label=region)
#qtmap(qstates, long, lat, group)


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

qtmap(qiowa, long, lat, group, label = subregion)


library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)
library(ggplot2)

#world <- map_data('world')
data(world)
qworld <- qdata(world)


qtmap(qworld, long, lat, group, label = id)


library(profr)

#res <- profr(print(qtmap(qworld, long, lat, group,
#   label=id)))}














































































