\name{link}
\alias{link}
\title{Link two mutaframes by a common categorical variable.}
\usage{link(mf1, mf2)

}
\description{Link two mutaframes by a common categorical variable.}
\details{This function links two mutaframes together by a common
categorical variable; this linking variable can be specified with
\code{\link{link_var}}.}
\value{the two mutaframes will be linked together by their
linking variables (listeners added)}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{mf1}{the first mutaframe}
\item{mf2}{the second mutaframe}
}
\examples{### (1) proof of concept
library(cranvas)

## link the original data with a frequency table
data1 = qmutaframe(iris)
tab2 = as.data.frame(table(iris$Species))
colnames(tab2) = c("type", "freq")
data2 = qmutaframe(tab2)

data1
data2

## specify the linking variable respectively: link
#   'Species' with 'type'
link_var(data1) = "Species"
link_var(data2) = "type"

link(data1, data2)

## now we brush data1
focused(data1) = TRUE  # suppose data1 is on focus
data1$.brushed[1] = TRUE  # the first row is brushed
## check the changes: data2 has been updated
data1
data2

## and we move the mouse over data2
focused(data2) = TRUE
focused(data1) = FALSE  # data1 lost focus, of course
data2$.brushed[3] = TRUE  # we brush the category 'virginica'
data2
data1

data2$.brushed[1] = FALSE  # do not brush 'setosa'
data2
data1



### (2) linking two par-coords plots: one original
#   data, one aggregated
data(nrcstat)
nrcstat.agg = aggregate(nrcstat[, 10:19], list(Regional.Code = nrcstat$Regional.Code), 
    function(x) round(mean(x), 1))
qnrc = qmutaframe(nrcstat)
qnrc.agg = qmutaframe(nrcstat.agg)
## link by region
link_var(qnrc) = link_var(qnrc.agg) = "Regional.Code"
link(qnrc, qnrc.agg)

qparallel(qnrc, vars = c(8, 10:19))
qparallel(qnrc.agg)

brush_attr(qnrc.agg, ".label.show") = TRUE  # turn on labels}


















































