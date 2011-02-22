\name{record_selector}
\alias{record_selector}
\title{Select a subset of data through a GUI to be brushed}
\usage{record_selector(data, vars, gui.type = c("RGtk2", 
    "Qt"))

}
\description{Select a subset of data through a GUI to be brushed.}
\details{We use a \pkg{gWidgets}-based GUI to subset the data based on the
variable given. Specifically, we choose certain values of the
variable (using the mouse or keyboard) and all the observations
which have the same values on this variable will be brushed. This
selector can link to any plots based on a mutaframe.}
\value{NULL}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{data}{a mutaframe (ideally with a column \code{.brushed})}
\item{vars}{character or integer: the variable to be displayed in
the data selector (if not specified, the first non-numeric
variable will be used; if all columns are numeric, the first
column will be used)}
\item{gui.type}{the type of the \pkg{gWidgets} GUI: \code{'RGtk2'}
is based on the package \pkg{gWidgetsRGtk2}, and \code{'Qt'} is
based on \pkg{gWidgetsQt}}
}
\examples{if (require("gWidgetsRGtk2")) {
    ## old iris as the toy example
    qiris = qmutaframe(iris)
    qparallel(qiris)
    record_selector(qiris)
    
    ## NRC rankings
    data(nrcstat)
    qnrc = qmutaframe(nrcstat)
    qparallel(qnrc, vars = 10:13, main = "Overview of Rankings", 
        horizontal = FALSE)
    record_selector(qnrc, "Institution.Name")
    qparallel(qnrc, vars = 14:19, main = "Research, Student Support, Diversity")
    qparallel(qnrc, vars = 20:26, main = "Publication, Award, Time to Degree")
}}













