\name{map_qdata}
\alias{map_qdata}
\title{Create data for drawing maps}
\usage{
map_qdata(database, regions = ".", color = NA, border = "black")
}
\arguments{
  \item{database}{see \code{\link[maps]{map}}}

  \item{regions}{see \code{\link[maps]{map}}}

  \item{color}{colors of graphical elements (default dark
  gray) corresponding to rows of data; it can be a vector
  of valid R colors, or a name of variable in \code{data}
  (must be either a factor or a numeric variable), or an R
  expression to calculate colors; \code{color} is used to
  fill the interior of graphical elements}

  \item{border}{colors for the border of graphical elements
  (e.g. rectangles); \code{NA} means to suppress the
  border}
}
\value{
  A mutaframe of region names and labels, with an attribute
  \code{MapData} containing the coordinates of polygons.
}
\description{
  This function converts maps data in the \pkg{maps}
  package to a suitable format for \pkg{cranvas}.
}
\details{
  The function \code{\link[maps]{map}} is used to convert
  maps data to a list, then the region names are stored in
  a mutaframe created by \code{\link{qdata}}; the polygon
  coordinates are stored in an attribute \code{MapData}. If
  multiple polygons belong to the same upper-level region,
  the column \code{labels} will store the upper-level
  region names.
}
\examples{
library(cranvas)
map_qdata("state")
map_qdata("county", "iowa")
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
