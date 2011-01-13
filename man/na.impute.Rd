\name{na.impute}
\alias{na.impute}
\title{Data Imputation}
\usage{na.impute(x, method = "below.min")
}
\description{Data Imputation}
\details{Impute data by various methods.}
\value{the imputed data}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{x}{the numeric data matrix}
\item{method}{imputation method; one of the following:
\describe{
\item{below.min}{replace missing values by a value 20\% below the mininum}
}}
}
