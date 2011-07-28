\name{na.impute}
\alias{na.impute}
\title{Data Imputation}
\usage{
  na.impute(x, method = "below.min")
}
\arguments{
  \item{x}{the numeric data matrix}

  \item{method}{imputation method; one of the following:
  \describe{ \item{below.min}{replace missing values by a
  value 20\% below the mininum} }}
}
\value{
  the imputed data
}
\description{
  Impute data by various methods.
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}

