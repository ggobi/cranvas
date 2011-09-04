\name{qbar}
\alias{qbar}
\title{Draw a bar plot}
\usage{
qbar(x, data = last_data(), space = 0.1, main = "", 
    horizontal = FALSE, standardize = FALSE, xlim = NULL, ylim = NULL, 
    xlab = NULL, ylab = NULL)
}
\arguments{
  \item{x}{a variable name (will be coerced to a factor if
  it is not; \code{NA} will also be a level of the factor
  if the variable has any \code{NA}'s)}

  \item{data}{a mutaframe created by \code{\link{qdata}}
  (default to be \code{\link{last_data}()}, i.e. the lastly
  used data)}

  \item{space}{the space between bars proportional to the
  width of bars}

  \item{main}{the main title}

  \item{horizontal}{\code{TRUE} to draw a horizontal plot
  or \code{FALSE} (vertical)}

  \item{standardize}{logical: whether to standardize the
  height of each bar to 1}

  \item{xlim}{a numeric vector of length 2 (like
  \code{c(x0, x1)}) for x-axis limits; it will be
  calculated from the data limits if not specified
  (\code{NULL}). Note when \code{x0 > x1}, the axis
  direction will be reversed (i.e. from larger values to
  small values)}

  \item{ylim}{y-axis limits; similar to \code{xlim}}

  \item{xlab}{x-axis title}

  \item{ylab}{y-axis title}
}
\value{
  A bar plot
}
\description{
  This function creates a bar plot based on a categorical
  variable to show the counts of all categories. Another
  categorical variable can be used to further split each
  bar into sub-categories, which will make the bar plot a
  representation of a contingency table. The splitting
  variable can be specified in \code{\link{qdata}} using
  either the \code{color} argument or the \code{border}
  argument.
}
\details{
  All the common interactions like brushing and deleting
  are documented in \code{\link{common_key_press}}.

  In the identify mode (press the key \code{?} to toggle
  between brush and identify mode), the variable and its
  identified values are shown as a text label in the plot,
  along with the counts and proportion of the identified
  categories. If the bar plot is split by an additional
  categorical variable, it will also be shown in the label.

  A zero-count category is represented by a one-pixel
  rectangle, which is a useful visual hint to indicate the
  presence of this category.

  The x-axis (or y-axis when \code{horizontal = TRUE})
  tickmark locations are from 1 to \code{n} shifted to the
  right by 0.5 (i.e. 1.5, 2.5, ...), where \code{n} is the
  number of levels of the factor variable to be plotted.
}
\examples{
library(cranvas)
## tennis data
data(tennis)

qtennis <- qdata(tennis)
qbar(matches)

## NRC rankings
data(nrcstat)

qnrc <- qdata(nrcstat, color = Control)  # Control: public or private univ
qbar(Regional.Code, main = "Number of public and private colleges in each region")

qparallel(vars = 13:10, main = "Overview of Rankings", 
    glyph = "tick", horizontal = TRUE, boxplot = TRUE)

qbar(Regional.Code, horizontal = TRUE)  # horizontal plot
qbar(Regional.Code, standardize = TRUE)  # standardize to 1


## use border color to split the bars
qnrc2 <- qdata(nrcstat, color = "white", border = Control)
qbar(Regional.Code)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  Other plots: \code{\link{qboxplot}},
  \code{\link{qdensity}}, \code{\link{qhist}},
  \code{\link{qmap}}, \code{\link{qmosaic}},
  \code{\link{qmval}}, \code{\link{qparallel}},
  \code{\link{qspine}}, \code{\link{qtime}}
}
