\name{na_impute}
\alias{na_impute}
\title{Simple data imputation}
\usage{
na_impute(x, method = "below.min")
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
Impute data by some simple methods.
}
\author{
Yihui Xie <\url{http://yihui.name}>
}
