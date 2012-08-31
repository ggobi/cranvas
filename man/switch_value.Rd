\name{switch_value}
\alias{switch_value}
\title{Switch the values of two variables}
\usage{
  switch_value(a, b, envir = .GlobalEnv)
}
\arguments{
  \item{a}{the name of the first variable (character)}

  \item{b}{the name of the second variable (character)}

  \item{envir}{the environment of the variables \code{a}
  and \code{b}}
}
\value{
  \code{NULL}
}
\description{
  The values of two variables \code{a} and \code{b} are
  switched in an environment \code{envir}.
}
\details{
  This function can make it a little bit easier when a plot
  can be drawn vertically and horizontally, in which case
  we only need to switch some attributes in the x-axis and
  y-axis, and the code for actual drawing is not affected.
  The bar plot is such an example.
}
\examples{
x1 = 4:9; x2 = letters
switch_value('x1', 'x2')
x1; x2
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link[base]{get}}, \code{\link[base]{assign}}
}

