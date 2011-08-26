\name{color_pal<-}
\alias{color_label<-}
\alias{color_pal<-}
\alias{color_var<-}
\alias{fill_label<-}
\alias{fill_pal<-}
\alias{fill_var<-}
\alias{size_label<-}
\alias{size_pal<-}
\alias{size_var<-}
\title{Set palettes and variables to map data to aesthetics}
\usage{
color_pal(data) <- value

color_var(data) <- value

color_label(data) <- value

fill_pal(data) <- value

fill_var(data) <- value

fill_label(data) <- value

size_pal(data) <- value

size_var(data) <- value

size_label(data) <- value
}
\arguments{
  \item{data}{the data object}

  \item{value}{the palette (as a function mapping a data
  variable to graphical properties), the variable name (as
  a character scalar), or the label (as a character
  scalar)}
}
\value{
  The corresponding scale information in \code{data} is set
  to \code{value}.
}
\description{
  These functions provide ways to modify the palettes,
  variables to create aesthetics and their labels in a data
  object created by \code{\link{qdata}}. Currently
  supported aesthetics are about color, fill and size of
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
data(tennis)
qtennis <- qdata(tennis, color = Aces)

## FIXME: this is not a good example; better use scatter
#   plot
qparallel(10:14)  # variables including Aces

## now notice the color changes in the plot
color_pal(qtennis) <- div_gradient_pal()  # change to diverging gradient palette

color_var(qtennis) <- "First.Serves"  # change color variable from Aces to First.Serves

color_label(qtennis) <- "First Serves"  # change label; to be used in legend
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
