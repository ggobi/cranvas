\name{qlegend}
\alias{qlegend}
\title{Create a legend layer}
\usage{qlegend(parent, data, vertical = TRUE, sister, 
    ...)

}
\description{
This function creates a legend layer}
\value{a layer object}
\author{Heike Hofmann}
\arguments{\item{parent}{the parent layer (default to be \code{NULL}, which
means creating an independent layer with no parents, but it can be
added to a parent layer using the approach \code{parent[i, j] <-
child_layer})}
\item{data}{\code{NULL} means to use \code{scale}}
\item{vertical}{(default is \code{TRUE})}
\item{sister}{a sister layer beside or below which to draw the legend layer;
the limits of this layer will be used for the legend layer}
\item{...}{other arguments passed to \code{\link[qtpaint]{qlayer}}}
}
