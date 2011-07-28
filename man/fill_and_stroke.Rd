\name{fill_and_stroke}
\alias{fill_and_stroke}
\title{Fill and Stroke by Color}
\usage{
  fill_and_stroke(color = NULL, fill = NULL, stroke = NULL)
}
\arguments{
  \item{color}{color to be used for (possibly) both the
  fill and stroke}

  \item{fill}{fill to be used}

  \item{color}{stroke to be used}
}
\description{
  Set the fill and stroke by the color if they are already
  not defined
}
\examples{
fill_and_stroke(color = "red")
fill_and_stroke(fill = "red", stroke = "black")
fill_and_stroke(color = "red", stroke = "black")
fill_and_stroke(color = "red", fill = "black")
}
\author{
  Barret Schloerke
}
\keyword{internal}
