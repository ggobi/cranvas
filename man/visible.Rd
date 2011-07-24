\name{visible}
\alias{visible}
\title{Set or query the visibility of observations}
\usage{visible(data)
visible(data) <- value
}
\description{Set or query the visibility of observations.}
\details{\code{visible}: There is a column \code{.visible} to control the visibility of
observations. This can be useful for ``deleting'' certain
observations from the plot (set their visibility to \code{FALSE}).

}
\value{\code{visible}: returns the logical vector to control the visibility of
observations

\code{visible<-}: changes the logical vector of visibility (i.e., \code{data$.visible})


}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link{qdata}}}
\arguments{\item{data}{the mutaframe}
\item{value}{a logical vector of the length \code{nrow(data)}}
}
\examples{df <- qdata(iris)

visible(df)

visible(df) <- rep(c(TRUE, FALSE), c(100, 50))  # hide the last 50 obs

visible(df)}






\alias{visible<-}
