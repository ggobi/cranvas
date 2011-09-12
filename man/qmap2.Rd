\name{qmap2}
\alias{qmap2}
\title{Draw a map}
\usage{
qmap2(data = last_data(), linkto = NULL, linkby = NULL, 
    main = "", xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{data}{the map data created by
  \code{\link{map_qdata}}}

  \item{linkto}{a mutaframe to link to so that the colors
  and the brushed status are in sync with this data}

  \item{linkby}{the variable in the \code{linkto} data to
  be used as a linking variable (see
  \code{\link{link_cat}})}

  \item{main}{the main title}

  \item{xlab}{x-axis title}

  \item{ylab}{y-axis title}

  \item{xlim}{a numeric vector of length 2 (like
  \code{c(x0, x1)}) for x-axis limits; it will be
  calculated from the data limits if not specified
  (\code{NULL}). Note when \code{x0 > x1}, the axis
  direction will be reversed (i.e. from larger values to
  small values)}

  \item{ylim}{y-axis limits; similar to \code{xlim}}
}
\value{
  A map
}
\description{
  This function creates a map based on the data of polygon
  coordinates.
}
\details{
  Common interactions are documented in
  \code{\link{common_key_press}}. Currently the brushing
  may be slightly different with other plots: when a region
  contains multiple sub-regions (polygons), and the whole
  region will be brushed if any of the sub-regions is
  brushed.

  The other feature is that a map can be linked to other
  datasets so that colors and the brushed status can be
  obtained from an additional data. This is useful due to
  the special format of map data: usually it is a waste of
  memory to store colors for every single boundary points
  in each region.
}
\examples{
library(cranvas)

### (1) simple maps
## the world map
qworld <- map_qdata("world")
qmap2(qworld)

## US states
qstate <- map_qdata("state")
qmap2(qstate)

## a simple demonstration of colors
qstate$.color <- sample(c("red", "blue"), nrow(qstate), 
    replace = TRUE)

## US counties
qcounty <- map_qdata("county")
qmap2(qcounty)

## map of China
if (require("mapdata")) {
    qchina <- map_qdata("china")
    qmap2(qchina)  # oh my! obviously china map needs more love
}



### (2) maps linked to other datasets
data(crimes)
head(crimes)
qcrimes <- qdata(crimes, color = population)  # population mapped to colors

qparallel(names(crimes)[-c(1, 2)], data = qcrimes)  # par-coords without first 2 columns
qscatter(population, robbery, data = qcrimes)

## categorical linking by states; now brush on any plot and
#   see changes in other plots
qmap2(qstate, linkto = qcrimes, linkby = "state")

## linking should be automatically removed when map is
#   closed; if not, try remove_link(qcrimes);
#   remove_link(qstate)
}
\author{
  Heike Hofmann and Yihui Xie
}
