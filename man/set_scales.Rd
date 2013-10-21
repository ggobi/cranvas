\name{color_pal<-}
\alias{border_pal<-}
\alias{border_title<-}
\alias{border_var<-}
\alias{color_pal<-}
\alias{color_title<-}
\alias{color_var<-}
\alias{size_pal<-}
\alias{size_title<-}
\alias{size_var<-}
\title{Set palettes and variables to map data to aesthetics}
\usage{
color_pal(data) <- value

color_var(data) <- value

color_title(data) <- value

border_pal(data) <- value

border_var(data) <- value

border_title(data) <- value

size_pal(data) <- value

size_var(data) <- value

size_title(data) <- value
}
\arguments{
  \item{data}{the data object}

  \item{value}{the palette (as a function mapping a data
  variable to graphical properties), the variable name (as
  a character scalar), or the title (as a character
  scalar)}
}
\value{
  The corresponding scale information in \code{data} is set
  to \code{value}.
}
\description{
  These functions provide ways to modify the palettes,
  variables to create aesthetics and their titles in a data
  object created by \code{\link{qdata}}. Currently
  supported aesthetics are about color, border and size of
  graphical elements.
}
\details{
  All these information is called ``scales'' (in the
  \pkg{ggplot2} term) and stored in \code{attr(data,
  'Scales')}. Usually palette functions are from the
  \pkg{scales} package.
}
\examples{
library(cranvas)
library(scales)
qtennis <- qdata(tennis, color = aces)

qscatter(server.pts, first.serves, data = qtennis)
qparallel(10:14, data = qtennis)  # variables including Aces
qscatter(server.pts, aces, data = qtennis)


## now notice the color changes in the plot
color_pal(qtennis) <- div_gradient_pal()  # change to diverging gradient palette

color_var(qtennis) <- "first.serves"  # change color variable from Aces to First.Serves

color_title(qtennis) <- "First Serves"  # change title; to be used in legend

cranvas_off()
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
