\name{selected}
\alias{selected}
\title{Set or query the selected (brushed) observations.}
\usage{selected(data)
selected(data) <- value}
\description{Set or query the selected (brushed) observations.}
\details{\code{selected}: The column \code{.brushed} controls which observations are being
brushed (i.e. those \code{TRUE}'s are selected).

}
\value{\code{selected}: returns the logical vector corresponding to whether the
observations are selected or not

\code{selected<-}: sets the selected status of observations


}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link{qdata}}}
\arguments{\item{data}{the mutaframe}
\item{value}{a logical vector of the length \code{nrow(data)}}
}
\examples{df = qdata(iris)

selected(df)

selected(df) = rep(c(TRUE, FALSE), c(10, 140))  # brush the first 10 obs

selected(df)}
\alias{selected<-}

