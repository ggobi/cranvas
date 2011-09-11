\name{setMapColorByLabel}
\alias{setMapColorByLabel}
\title{Add Colour to Interactive Maps}
\usage{
setMapColorByLabel(qmap, qdata, label, scale = scale_colour_gradient())
}
\arguments{
  \item{qmap}{a mutaframe for the ap representation}

  \item{qdata}{is the mutaframe containing the variable to
  be displayed as colour}

  \item{label}{is the variable in the qdata mutaframe to be
  displayed}

  \item{scale}{is a color scheme from the scales package,
  options include scale_colour_gradient(),
  scale_colour_gradient2(), ...}
}
\description{
  Creates a choropleth map from an interactive map
  generated with qmap.
}
\details{
  'C' computes a cartogram representation
}
\author{
  Heike Hofmann
}
