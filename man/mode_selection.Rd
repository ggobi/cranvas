\name{mode_selection}
\alias{mode_selection}
\title{Logical operations under different selection mode}
\usage{
mode_selection(x, y, mode = "none")
}
\arguments{
  \item{x}{logical: the previous selection status}

  \item{y}{logical: the current selection status (if
  \code{y} is a numeric vector, it will be converted to a
  logical vector of the same length with \code{x} with
  \code{TRUE}'s corresponding to the numeric indicies)}

  \item{mode}{the selection mode string; see Details}
}
\value{
a logical vector indicating whether the objects are selected
}
\description{
A selection mode is essentially a logical operation like AND, OR, and XOR,
etc.
}
\details{
There are five selection modes: \describe{ \item{none}{ignore previous
selection and completely start over again} \item{and}{select the
intersection, i.e. the objects that are selected by two successive brushing
operations} \item{or}{select the union, i.e. any objects selected by all
previous operations and the current operation} \item{xor}{toggle the
selection} \item{not}{negation, i.e. exclude the objects under two successive
brushing operations} \item{complement}{the complement of the current
selection} } We can hold the key while brushing: A for 'and', O for 'or', X
for 'xor', N for 'not' and C for 'complement'.
}
\examples{
x1 <- c(TRUE, TRUE, FALSE, FALSE)
x2 <- c(FALSE, TRUE, TRUE, FALSE)
mode_selection(x1, x2, "none")
mode_selection(x1, x2, "and")
mode_selection(x1, x2, "or")
mode_selection(x1, x2, "xor")
mode_selection(x1, x2, "not")
mode_selection(x1, x2, "complement")

mode_selection(x1, c(2, 3), "and")  # equivalent to x2
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
\seealso{
\code{\link[base]{&}}, \code{\link[base]{|}},
  \code{\link[base]{xor}}, \code{\link[base]{!}}
}
