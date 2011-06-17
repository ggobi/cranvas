\name{record_selector}
\alias{record_selector}
\title{Select a subset of data through a GUI to be brushed.}
\usage{record_selector(data, vars)
}
\description{Select a subset of data through a GUI to be brushed.}
\details{We use a GUI based on \pkg{gWidgetsQt} to subset the data based on
the variable given. Specifically, we choose certain values of the
variable (using the mouse or keyboard) and all the observations
which have the same values on this variable will be brushed. This
selector can link to any plots based on a mutaframe created by
\code{\link{qdata}}.}
\value{\code{NULL} (a GUI will pop up if the \pkg{gWidgetsQt}
package is available)}
\author{Yihui Xie <\url{http://yihui.name}>}
\seealso{\code{\link{qdata}}}
\arguments{\item{data}{a mutaframe created by \code{\link{qdata}} (with a
column \code{.brushed})}
\item{vars}{character or integer: the variable to be displayed in
the data selector (if not specified, the first non-numeric
variable will be used; if all columns are numeric, the first
column will be used)}
}
\examples{## old iris as the toy example
qiris = qdata(iris)
qparallel(data = qiris)
record_selector(qiris)

## NRC rankings
data(nrcstat)
qnrc = qdata(nrcstat)
qparallel(vars = 10:13, data = qnrc, main = 'Overview of Rankings', horizontal=FALSE)
record_selector(qnrc, 'Institution.Name')
qparallel(vars = 14:19, data = qnrc, main = 'Research, Student Support, Diversity')
qparallel(vars = 20:26, data = qnrc, main = 'Publication, Award, Time to Degree')}

