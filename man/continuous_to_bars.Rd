\name{continuous_to_bars}
\alias{continuous_to_bars}
\title{Continuous items to bins}
\usage{
  continuous_to_bars(data = NULL, splitBy = NULL, brushed =
  NULL, typeInfo = "hist", position = "none", ...)
}
\arguments{
  \item{data}{data to be used}

  \item{splitBy}{vect to split by}

  \item{brushed}{vect to brush by}

  \item{typeInfo}{typeInfo$type ENUM of 'hist', 'ash',
  'dot', 'spine', 'density'}

  \item{position}{enum{'none', 'stack', 'dodge',
  'relative', 'identity'}}

  \item{stroke}{vect to outline by}

  \item{...}{other params passed to
  \code{\link[graphics]{hist}}}
}
\description{
  Continuous items to bins
}
\examples{
temp_breaks <- hist(mtcars$disp, plot = FALSE)$breaks[1:2]
type <- list(type = "hist", binwidth = diff(temp_breaks), 
    start = temp_breaks[1])
continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, 
    stroke = "black")
continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, 
    position = "dodge", stroke = "black")
continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, 
    position = "identity", stroke = "black")
continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, 
    position = "relative", stroke = "black")
continuous_to_bars(mtcars$disp, mtcars$cyl, typeInfo = type, 
    position = "stack", stroke = "black")
}
\author{
  Barret Schloerke
}
\keyword{internal}
