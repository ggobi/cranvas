\name{reorder_var}
\alias{reorder_var}
\title{Re-order the columns of a data frame based on MDS or ANOVA}
\usage{
  reorder_var(data,
    type = c("none", "MDS", "ANOVA", "randomForest"),
    vars = names(data), numcol = sapply(data, is.numeric),
    x = data$.color)
}
\arguments{
  \item{data}{a data frame (or similar data structures like
  mutaframes)}

  \item{type}{the method to re-order the variables
  (columns)}

  \item{vars}{the column names of the \code{data}}

  \item{numcol}{a logical vector indicating which columns
  are numeric}

  \item{x}{the x variable to be used in ANOVA and
  randomForest}
}
\value{
  the column names (i.e. the argument \code{vars}) after
  being re-ordered; note non-numeric variables will always
  be put in the end and they will not go into the
  computation
}
\description{
  For the MDS method, we use (1 - correlation matrix) as
  the distance matrix and re-order the columns according
  their distances between each other (i.e. 1-dimension
  representation of p-dimension); as a result, columns in a
  neighborhood indicate they are more similar to each
  other. For the ANOVA method, if there is a column named
  \code{.color}, it will be used as the x variable and we
  perform ANOVA on each numeric column vs this variable,
  then the columns are re-ordered by the P-values, so the
  colors can discriminate the first few columns most apart.
  Of course, when there is only a single color in the
  \code{.color} variable, the ANOVA method will not work
  and the original order will be returned. For the
  randomForest method, the variables will be ordered by the
  importance scores (mean descrease in accuracy) and the
  argument \code{x} will be used as the response variable.
}
\examples{
data(tennis)
reorder_var(tennis, type = 'MDS')

reorder_var(iris, type = 'ANOVA', x = iris$Species)
names(iris)  # original column names
reorder_var(iris, type = 'randomForest', x = iris$Species)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}

