\name{link_knn}
\alias{link_knn}
\title{k-Nearest neighbor linking}
\usage{
link_knn(mf1, var1 = NULL, mf2 = NULL, var2 = var1, k = 10)
}
\arguments{
  \item{var1}{the variable names or column indices of the
  first mutaframe to be used to calculate distances}

  \item{var2}{(optional) variable names or column indices
  of the second mutaframe (by default the same as
  \code{var1})}

  \item{k}{the number of nearest neighbors to select}

  \item{mf1}{the first mutaframe}

  \item{mf2}{(optional) the second mutaframe; default
  \code{NULL} means \code{mf1} will be linked to itself}
}
\value{
  Similar to categorical linking (\code{\link{link_cat}}),
  this function also links two mutaframes together (or one
  mutaframe to itself), and id's of listeners are returned.
}
\description{
  When a number of elements in a data are brushed, their
  k-nearest neighbors (based on a certain distance measure)
  are brushed as well.
}
\details{
  A center point for the variables based on the selected
  rows is calculated in the first dataset, then the k
  nearest rows in the second dataset (if not provided, it
  will be the same as the first dataset) to this center are
  selected. Only the Euclidean distance has been
  implemented at the moment.
}
\examples{

### (1) linking to oneself
data(flea, package = "tourr")
qflea <- qdata(flea, color = species)
qscatter(tars1, tars2, data = qflea)

## brush the nearest 10 points around the center
id <- link_knn(qflea, c("tars1", "tars2"), k = 10)

## remove the linking
remove_link(qflea, id)


### (2) variables on the same scale
mf <- qdata(data.frame(x = rnorm(300), y = rnorm(300)), size = 3)
qscatter(x, y, data = mf)

id <- link_knn(mf, c("x", "y"), k = 20)

remove_link(mf, id)


### (3) link two datasets

mf1 <- qdata(flea, color = species)
mf2 <- qdata(subset(flea, species == "Concinna "))

qscatter(tars1, tars2, data = mf1)
qscatter(tars1, tars2, data = mf2)

id <- link_knn(mf1, c("tars1", "tars2"), mf2, c("tars1", "tars2"))

remove_link(mf1, id[1])
remove_link(mf2, id[2])

cranvas_off()
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
