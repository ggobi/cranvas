\docType{data}
\name{wages.demog}
\alias{wages.demog}
\title{Demographic data for wages of male high-school dropouts}
\format{Number of subjects: 888; Number of variables: 6; Number of
  observations, across all subjects: 888

  \describe{ \item{\code{id}}{id numbers for each subject}
  \item{\code{ged}}{if a graduate equivalency diploma ever is obtained}
  \item{\code{black}}{categorical indicator of race is black}
  \item{\code{hispanic}}{categorical indicator of race is hispanic}
  \item{\code{hgc}}{highest grade completed} \item{\code{race}}{categorical
  variable, either white, hispanic or black}}}
\source{
  Singer, J. D. & Willett, J. B. (2003), \emph{Applied
  Longitudinal Data Analysis}, Oxford University Press,
  Oxford, UK. It is a subset of data collected in the
  National Longitudinal Survey of Youth (NLSY) described at
  \url{http://www.bls.gov/nls/nlsdata.htm}.
}
\description{
  This is just the demographic data for each person
  recorded in the wages data.
}
\examples{
library(cranvas)

### (1) linking to between two tables, using common id variable

qwg <- qdata(wages.demog)
# qscatter(ged, race, data=qwg)
qbar(race, data = qwg)
qbar(ged, data = qwg)
qhist(hgc, data = qwg)

qwages <- qdata(wages)
qscatter(exper, lnw, data = qwages, alpha = 0.5)

id <- link_cat(qwages, var1 = "id", qwg, var2 = "id")

remove_link(qwages, id[1])
remove_link(qwg, id[2])

### (1.5) linking between two datasets, using several id variables
id <- link_cat(qwages, var1 = c("black", "hispanic"), qwg, var2 = c("black", "hispanic"))

qscatter(exper, lnw, data = qwages, alpha = 0.5)
qbar(black, data = qwg)
qbar(hispanic, data = qwg)

remove_link(qwages, id[1])
remove_link(qwg, id[2])

### (2) linking to oneself through a categorical variable
data(flea, package = "tourr")

qflea <- qdata(flea, color = species)
qhist(tars1, data = qflea)  # an ordinary histogram; try brushing

## now we link qflea to itself by species
id <- link_cat(qflea, "species")
## brush the plot and see what happens

remove_link(qflea, id)  # remove this linking; back to normal linking again

### (2.5) link to oneself by several categorical variables
idmulti <- link_cat(qwages, c("ged", "black", "hispanic"))

qscatter(exper, lnw, data = qwages, alpha = 0.5)

remove_link(qwages, idmulti)

### (3) link the original data with a frequency table
tab2 <- as.data.frame(table(flea$species))
colnames(tab2) <- c("type", "freq")
(qflea2 <- qdata(tab2))
head(qflea)  # what the two datasets look like

## see how two different datasets can be linked through a common categorical
## variable
id <- link_cat(qflea, var1 = "species", qflea2, var2 = "type")
qhist(tars1, data = qflea)
qbar(type, data = qflea2, standardize = TRUE)

## remove the linking on two datasets respectively
remove_link(qflea, id[1])
remove_link(qflea2, id[2])
}
\keyword{datasets}
