\name{link_cat}
\alias{link_cat}
\title{Categorical linking}
\usage{
link_cat(mf1, var1, mf2 = NULL, var2 = NULL)
}
\arguments{
  \item{mf1}{the first mutaframe}

  \item{var1}{the name of the linking variable of
  \code{mf1}}

  \item{mf2}{(optional) the second mutaframe; default
  \code{NULL} means \code{mf1} will be linked to itself}

  \item{var2}{(optional) the name of the linking variable
  of \code{mf2}}
}
\value{
  The mutaframes will be linked together by their linking
  variables (listeners are added to mutaframes), and the
  id's of the listeners attached on each mutaframe will be
  returned as a vector (first element for the first
  mutaframe; second element for the second one).
}
\description{
  This function links two mutaframes together (or one
  mutaframe to itself) by a common categorical variable so
  that whenever one element (or multiple elements) in a
  category (or multiple categories) is brushed, all
  elements in this (these) categories will be brushed.
}
\details{
  Categorical linking is achieved by a series of logical
  operations: first, look for which rows are brushed in the
  first mutaframe, and find out the values of its linking
  variable as well as the categories they belong to, then
  look for which elements of the linking variable in the
  second mutaframe (possibly the same mutaframe) are in
  these categories, and brush these elements (corresponding
  to rows).

  The implementation is through listeners on mutaframes
  from the \pkg{plumbr} package. It may be important keep
  track of the id's of listeners to avoid unnecessary
  burden of updating data objects in a linking chain.
  Listeners can be detached from mutaframes by
  \code{\link[plumbr]{remove_listener}} (see examples
  below).
}
\examples{
library(cranvas)
data(flea, package = "tourr")

### (1) linking to oneself through a categorical variable
qflea <- qdata(flea, color = species)
qhist(tars1)  # an ordinary histogram; try brushing

## now we link qflea to itself by species
id <- link_cat(qflea, "species")
## brush the plot and see what happens

remove_listener(qflea, id)  # remove this linking; back to normal linking again


### (2) link the original data with a frequency table
tab2 <- as.data.frame(table(flea$species))
colnames(tab2) <- c("type", "freq")
(qflea2 <- qdata(tab2))
head(qflea)  # what the two datasets look like

## see how two different datasets can be linked through a
#   common categorical variable
id <- link_cat(qflea, var1 = "species", qflea2, var2 = "type")
qhist(tars1, data = qflea)
qbar(type, data = qflea2, standardize = TRUE)


## remove the linking on two datasets respectively
remove_listener(qflea, id[1])
remove_listener(qflea2, id[2])
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link{qdata}}, \code{\link{link_var}},
  \code{\link{link_type}}
}
