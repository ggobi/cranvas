\name{var_names}
\alias{var_names}
\alias{var_names.character}
\alias{var_names.default}
\alias{var_names.formula}
\alias{var_names.numeric}
\title{Get variable names}
\usage{
  var_names(vars, data)

  \method{var_names}{default} (vars, data)

  \method{var_names}{character} (vars, data)

  \method{var_names}{numeric} (vars, data)

  \method{var_names}{formula} (vars, data)
}
\arguments{
  \item{vars}{a character vector of variable names, or a
  numeric vector of column indices, or a two-sided formula
  like \code{~ x1 + x2 + x3}}

  \item{data}{the data containing the variables}
}
\value{
  A character vector
}
\description{
  It is often flexible to input variables in plotting
  functions, and this generic function convert most common
  inputs (character, numeric or formula) to a character
  vector of variable names.
}
\details{
  Numeric indices are converted to character names
  according to the positions of variables in the data;
  \code{\link[base]{all.vars}} is used to extract all
  variable names in a formula, and the special formula
  \code{~ .} is treated differently: it means all variables
  in the data except thoese names starting with a dot (e.g.
  \code{.color}).
}
\examples{
var_names(~., mtcars); var_names(~disp+hp, mtcars); var_names(1:3, mtcars)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}

