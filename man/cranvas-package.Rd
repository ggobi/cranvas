\docType{package}
\name{cranvas-package}
\alias{cranvas}
\alias{cranvas-package}
\title{Interactive statistical graphics based on Qt}
\description{
  This package was designed mainly for interactions in
  statistical plots, a feature (nearly) missing in \R for
  long. It contains most common statistical plots like
  histograms, scatter plots, bar charts, parallel
  coordinates plots, density plots, mosaic plots, boxplots,
  maps, missing value plots and time series plots. All
  plots support some common interactions as well as
  plot-specific interactions with the keyboard or the
  mouse.
}
\details{
  The actual drawing is based on two packages \pkg{qtbase}
  and \pkg{qtpaint}, which connect \R to Qt. The data
  structure is based on \pkg{plumbr} and
  \pkg{objectSignals}; the \code{\link[plumbr]{mutaframe}}s
  and reference classes are used extensively in this
  package. Usually there are listeners
  (\code{\link[plumbr]{add_listener}}) and signaling fields
  (\code{\link[objectSignals]{signalingFields}}) attached
  to data objects (created by \code{\link{qdata}}), so the
  plots can listen to the changes in data (hence get
  updated). Note all the plots based on the same data
  object are linked by default, so the interactions in one
  plot will be reflected in other plots as well.

  A plot can be in either the brush mode (default) or the
  identify mode. In the brush mode, we can use a rectangle
  brush to select elements in the plot; in the identify
  mode, the information about the identified elements under
  the mouse will be shown in the plot.

  See \code{\link{common_mouse_press}},
  \code{\link{common_mouse_move}},
  \code{\link{common_mouse_release}},
  \code{\link{common_key_press}} and
  \code{\link{common_key_release}} for common interactions
  in all plots, and the documentation of specific plots for
  other possible interactions.
}
\examples{
library(cranvas)
data(tennis)
## the first step is always to create the data object
qtennis <- qdata(tennis, color = Serve.Speed, fill = Serve.Speed, 
    size = Aces)

## then create plots
qhist(Serve.Speed, data = qtennis)

qbar(Country, data = qtennis)

qparallel(~., data = qtennis)  # all variables; you may need a large window

record_selector("Name", data = qtennis)  # a simple GUI
}
