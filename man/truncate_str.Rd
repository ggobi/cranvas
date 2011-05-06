\name{truncate_str}
\alias{truncate_str}
\title{Truncate character strings}
\usage{truncate_str(x, length, extra = "...")

}
\description{Truncate character strings.}
\details{Truncate a string if its length is greater than a specified
\code{length}, with an extra flag appended in the end.}
\value{the truncated strings}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{x}{a character vector}
\item{length}{the desired length}
\item{extra}{the characters to be appended to the strings if their
lengths are greater than the specified \code{length}}
}
\examples{truncate_str("asdfasdf", 5)
truncate_str(c("asdf", "qwer"), 2)
truncate_str(c("asdf", "qwer"), c(1, 4))
truncate_str(c("asdf", "qwer"), 3, "??")}



