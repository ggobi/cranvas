\docType{data}
\name{wages}
\alias{wages}
\title{Wages of male high-school dropouts}
\format{Number of subjects: 888; Number of variables: 15; Number
of observations, across all subjects: 6402

\describe{ \item{\code{id}}{id numbers for each subject}
\item{\code{lnw}}{natural log of wages, adjusted for inflation, to
1990 dollars} \item{\code{exper}}{length of time in the workforce
(in years). This is treated as the time variable, with \eqn{t = 0}
for each subject starting on their first day at work. The number
of time points and values of time points for each subject can
differ} \item{\code{ged}}{when/if a graduate equivalency diploma
is obtained} \item{\code{black}}{categorical indicator of race is
black} \item{\code{hispanic}}{categorical indicator of race is
hispanic} \item{\code{hgc}}{highest grade completed}
\item{\code{uerate}}{unemployment rates in the local geographic
region at each measurement time} }}
\source{
  Singer, J. D. & Willett, J. B. (2003), \emph{Applied
  Longitudinal Data Analysis}, Oxford University Press,
  Oxford, UK. It is a subset of data collected in the
  National Longitudinal Survey of Youth (NLSY) described at
  \url{http://www.bls.gov/nls/nlsdata.htm}.
}
\usage{
data(wages)
}
\description{
  The data was collected to track the labor experiences of
  male high-school dropouts. The men were between 14 and 17
  years old at the time of the first survey.
}
\examples{
library(cranvas)
data(wages)

\dontrun{
## this is a very simple linear regression assuming each man has the same
## slope, which may not be entirely reasonable
fit <- lm(lnw ~ exper + id, data = wages)
wage2 <- subset(wages, !duplicated(wages$id), c(id, hispanic))
## predict wages at year 0 and 6 respectively
wage2$lnw0 <- predict(fit, data.frame(id = unique(wages$id), exper = 0))
wage2$lnw6 <- predict(fit, data.frame(id = unique(wages$id), exper = 6))
qwage2 <- qdata(wage2, color = hispanic)

qhist(lnw0, main = "Wages on the first day")
qhist(lnw6, main = "Wages after 6 years")
}
}
\keyword{datasets}
