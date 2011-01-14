\name{mode_selection}
\alias{mode_selection}
\title{Logical Operations Under Different Selection Mode}
\usage{mode_selection(x, y, mode = "none")
}
\description{Logical operations under different selection mode.}
\details{There are five selection modes:
\describe{
\item{none}{ignore previous selection and completely start over again}
\item{and}{select the intersection, i.e. the objects that are selected by two successive brushing operations}
\item{or}{select the union, i.e. any objects selected by all previous operations and the current operation}
\item{xor}{toggle the selection}
\item{not}{negation, i.e. exclude the objects under two successive brushing operations}
\item{complement}{the complement of the current selection}
}
We can hold the key while brushing: A for 'and', O for 'or', X for 'xor', N for 'not' and C for 'complement'.}
\value{a logical vector indicating whether the objects are selected}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{x}{logical: the previous selection status}
\item{y}{logical: the current selection status}
\item{mode}{the selection mode string; see Details}
}
\examples{x1 = c(TRUE, TRUE, FALSE, FALSE)
x2 = c(FALSE, TRUE, TRUE, FALSE)
mode_selection(x1, x2, "none")
mode_selection(x1, x2, "and")
mode_selection(x1, x2, "or")
mode_selection(x1, x2, "xor")
mode_selection(x1, x2, "not")
mode_selection(x1, x2, "complement")}







