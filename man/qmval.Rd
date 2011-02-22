\name{qmval}
\alias{qmval}
\title{Interactive Missing Value Plot}
\usage{qmval(data, vars, main, ...)

}
\description{Interactive Missing Value Plot.}
\value{NULL}
\author{Heike Hofmann}
\arguments{\item{data}{a mutaframe which is typically built upon a data frame
along with several row attributes}
\item{...}{}
}
\examples{library(qtbase)
library(qtpaint)
library(cranvas)


library(maps)
library(ggplot2)

states <- map_data("state")
qstates <- qmutaframe(states)

#qtmap(qstates, long, lat, group)


#######################

crimes <- read.csv("http://www.hofroe.net/stat579/crimes-09.csv")
crimes$State <- tolower(crimes$State)

## color palette
library(RColorBrewer)
crimes$nines <- with(crimes, cut(Burglary/Population, 
    9))
#crimes$nines <- with(crimes,
#   cut(Violent.crime/Population, 9))
crimes$nines <- brewer.pal(9, "Greys")[crimes$nines]
qcrimes <- qmutaframe(crimes)
print(qparallel(qcrimes))

source("map.R")
print(qtmap(qstates, long, lat, group, label = region, 
    labeldata = qcrimes, by.x = "region", by.y = "State", 
    colour = Violent.crime))
q <- qtmap(qstates, long, lat, group, label = region, 
    labeldata = qcrimes, by.x = "region", by.y = "State", 
    colour = log(Violent.crime/Population + 1))


#qtmap(qstates, long, lat, group, label=region)
#qtmap(qstates, long, lat, group)




#world <- map_data('world')
#qworld <- qmutaframe(world)

#qtmap(qworld, long, lat, group)}









































