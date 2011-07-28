\name{break_str}
\alias{break_str}
\title{Insert line breaks into character strings}
\usage{
  break_str(x, split = "[^[:alnum:]]", ...)
}
\arguments{
  \item{x}{a character vector}

  \item{split}{the rule (regular expression) to replace
  characters by line breaks}

  \item{...}{other arguments passed to
  \code{\link[base]{gsub}}}
}
\value{
  a character vector with certain characters replaced by
  \code{'\n'}
}
\description{
  By default, all the non-alphanumeric characters are
  replaced by \code{'\n'}, which can be useful when
  plotting long axis labels, e.g., in parallel coordinates
  plots.
}
\examples{
break_str(c("long label1.1", "long label1.2", "long label1.3"), 
    split = " ")
break_str(names(iris))
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
