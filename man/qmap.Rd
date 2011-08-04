\name{qmap}
\alias{qmap}
\title{Interactive Maps}
\usage{
  qmap(data, longitude, latitude, group, label = group,
  main = NULL, filled = TRUE, ...)
}
\arguments{
  \item{data}{a mutaframe which is typically built upon a
  data frame along with several row attributes}

  \item{latitude}{spatial x variable}

  \item{longitude}{spatial y variable}

  \item{group}{grouping variable for polygons}

  \item{main}{window title, by default 'Map of <data set>'}

  \item{...}{}
}
\value{
  NULL
}
\description{
  Create an interactive map from qdata.
}
\details{
  mouse wheel events are translated to focal zoom

  'R' resets maps to original scale after zoom
}
\examples{
library(cranvas)
library(maps)
library(ggplot2)

states <- map_data("state")
states$region <- factor(states$region)
qstates <- qdata(states)

qstates$.color <- sample(c("red", "blue"), nrow(states), 
    replace = TRUE)
print(qmap(qstates, long, lat, group, region))

# recolor
qstates$.color <- sample(c("red", "blue"), nrow(states), 
    replace = TRUE)

#######################

data(crimes)
crimes$State <- factor(tolower(crimes$State))

qcrimes <- qdata(crimes)
print(qparallel(data = qcrimes))

# now link map and crimes data set
link_var(qcrimes) <- "State"
link_var(qstates) <- "region"

link(qcrimes, qstates)
print(qscatter(Population, Robbery, qcrimes))

# Choropleth maps
setMapColorByLabel(qstates, qcrimes, Robbery/Population, 
    scale_colour_gradient())

# change to different choropleth map:
setMapColorByLabel(qstates, qcrimes, log(1e+05 * Robbery/Population + 
    1), scale_colour_gradient2())
setMapColorByLabel(qstates, qcrimes, log(1e+05 * Burglary/Population + 
    1), scale_colour_gradient2())

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

#res <- profr(print(qmap(qworld, long, lat, group,
#   label=id)))


#################
# cartogram
# first example

data(cartdata)
# cartdata is a cartogram for the rate of burglaries from
#   the crimes data set
# the cartogram algorithm is based on the one implemented
#   in the cart package (R Forge)

# cartdata should have same length as qstates

cartdata$diffx <- cartdata$long - states$long
cartdata$diffy <- cartdata$lat - states$lat

# map to cartogram
for (i in 11:1) {
    x <- (i - 1)/10
    qstates$long <- with(cartdata, long - x * diffx)
    qstates$lat <- with(cartdata, lat - x * diffy)
    scan()
}

# and back to the map again
for (i in 1:11) {
    x <- (i - 1)/10
    qstates$long <- with(cartdata, long - x * diffx)
    qstates$lat <- with(cartdata, lat - x * diffy)
    scan()
}


}
\author{
  Heike Hofmann
}
