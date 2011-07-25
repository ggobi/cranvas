\name{qlegend}
\alias{qlegend}
\title{Create a legend layer}
\usage{qlegend(parent, data, vertical = TRUE, ...)

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
\item{...}{other arguments passed to \code{\link[qtpaint]{qlayer}}}
}
