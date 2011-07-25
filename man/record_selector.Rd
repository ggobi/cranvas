\name{record_selector}
\alias{record_selector}
\title{Select a subset of data through a GUI to be brushed}
\usage{record_selector(vars, data)

}
\description{
We use a GUI created by \pkg{qtbase} to subset the data based on a
given categorical variable. Specifically, we choose certain values
of the variable (using the mouse or keyboard) and all the
observations which have the same values on this variable will be
brushed. This selector can link to any plots based on a mutaframe
created by \code{\link{qdata}}.}
\details{The GUI supports multiple selections when we hold the Shift or
Ctrl key. If the character string in the text input box matches
with multiple items in the list, all of them will be selected.

When we select items in the list, usually a plot based on the same
data will get brushed accordingly. On the other hand, when we
click on a plot, the corresponding items in the list will be
selected as well.}
\value{\code{NULL} (a GUI will pop up)}
\author{Yihui Xie and Jason Crowley}
\seealso{\code{\link{qdata}}}
\arguments{\item{data}{a mutaframe created by \code{\link{qdata}} (with a
column \code{.brushed})}
\item{vars}{character or integer: the variable to be displayed in
the data selector (if not specified, the first non-numeric
variable will be used; if all columns are numeric, the first
column will be used)}
}
\examples{library(cranvas)

## old iris as the toy example
qiris <- qdata(iris)
qparallel(data = qiris)
record_selector(data = qiris)

## NRC rankings
data(nrcstat)
qnrc <- qdata(nrcstat)
qparallel(vars = 10:13, data = qnrc, main = "Overview of Rankings", 
    horizontal = FALSE)
record_selector("Institution.Name", qnrc)
qparallel(vars = 14:19, data = qnrc, main = "Research, Student Support, Diversity")
qparallel(vars = 20:26, data = qnrc, main = "Publication, Award, Time to Degree")}













