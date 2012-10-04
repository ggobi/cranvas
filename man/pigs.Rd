\docType{data}
\name{pigs}
\alias{pigs}
\title{Temporal measurements on UK Pig production}
\format{data.frame: 48 obs. of 11 variables \describe{\item{\code{TIME}}{Time
  index, from 1 to 48} \item{\code{YEAR}}{Year, from 1967 to 1978}
  \item{\code{QUARTER}}{Quarter index, take value in 1,2,3,4}
  \item{\code{Q1}}{Whether is the first quarter} \item{\code{Q2}}{Whether is
  the second quarter} \item{\code{Q3}}{Whether is the third quarter}
  \item{\code{GILTS}}{Number of sows in pig for the first time}
  \item{\code{PROFIT}}{Ratio of all-pig price to all fattener feed price}
  \item{\code{S.HERDSZ}}{Ratio of sow and boar slaughter to total breeding
  herd size} \item{\code{PRODUCTION}}{Number of clean pig meat slaughtered}
  \item{\code{HERDSZ}}{Actual breeding herd size}}}
\source{
  Andrews, D.F. and Herzberg, A.M. (1985), \emph{Data - A
  Collection of Problems from Many Fields for the Student
  and Research Worker}, Springer-Verlag, New York, NY. URL:
  \url{http://lib.stat.cmu.edu/datasets/Andrews/}
}
\description{
  Multivariate time series data originally from Andrews and
  Herzberg (1985).
}
\examples{
library(cranvas)
data(pigs)
qpig <- qdata(pigs)
qtime(TIME, ~GILTS + PROFIT + PRODUCTION + HERDSZ, qpig, shift = c(1, 4))
}
\keyword{datasets}
