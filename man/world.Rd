\docType{data}
\name{world}
\alias{world}
\title{Coordinates of the world map}
\format{data.frame: 48 obs. of 11 variables}
\source{
  maps package
}
\usage{
data(world)
}
\description{
  An shortened version of the map coordinates for all the
  countries on the globe. Polygon edges will be a bit
  rough, but the speed is improved for interaction.
}
\examples{
library(cranvas)
data(world)
qworld <- map_qdata("world")
qmap(qworld)
}
\keyword{datasets}
