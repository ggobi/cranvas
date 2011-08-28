\name{qbar}
\alias{qbar}
\title{Draw a bar plot}
\usage{
qbar(x, data = last_data(), space = 0.1, main = "", 
    horizontal = FALSE)
}
\arguments{
  \item{x}{a variable name (will be coerced to a factor if
  it is not; \code{NA} will also be a level of the factor
  if the variable has any \code{NA}'s)}

  \item{data}{a mutaframe created by \code{\link{qdata}}}

  \item{space}{the space between bars proportional to the
  width of bars}

  \item{main}{the main title}

  \item{horizontal}{\code{TRUE} to draw a horizontal plot
  or \code{FALSE} (vertical)}
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
}
\examples{
library(cranvas)
## tennis data
data(tennis)

qtennis <- qdata(tennis)
qbar(Matches)

## NRC rankings
data(nrcstat)

qnrc <- qdata(nrcstat, color = Control)  # Control: public or private univ
qbar(Regional.Code, main = "Number of public and private colleges in each region")

qparallel(vars = 13:10, main = "Overview of Rankings", 
    glyph = "tick", horizontal = FALSE, boxplot = TRUE)

qbar(Regional.Code, horizontal = TRUE)  # horizontal plot

## use border color to split the bars
qnrc2 <- qdata(nrcstat, color = "white", border = Control)
qbar(Regional.Code)
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
