\name{qmosaic}
\alias{qmosaic}
\title{Mosaic plot.
Create a mosaicplot using a formula (as described in prodplot)}
\usage{
qmosaic(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, 
    subset = NULL, colour = "grey30", main = NULL, ...)
}
\arguments{
  \item{data}{a mutaframe which is typically built upon a
  data frame along with several row attributes}

  \item{formula}{a formula to describe order in which
  variables go into the mosaicplot. The first variables are
  the ones visually most important, i.e. Y ~ X1 + X2 + X3
  first splits according to X3, then according to X2, then
  according to X1}

  \item{divider}{structure of the split in each direction.
  Choices are "hbar", "vbar" for horizontal/vertical
  barcharts, "hspine", "vspine" for horizontal/vertical
  spineplots.}

  \item{cascade}{parameter for prodplot in package
  productplots}

  \item{scale_max}{parameter for prodplot in package
  productplots}

  \item{na.rm}{handling of missing values, defaults to
  FALSE}

  \item{subset}{parameter for prodplot -}

  \item{colour}{fill colour of rectangles - only used if
  colour is not used in the data}

  \item{main}{parameter for prodplot}

  \item{...}{}
}
\value{
  NULL
}
\description{
  Interactive elements for mosaic plots are arrow keys for
  navigating through the mosaic hierarchy: arrow up reduces
  complexity of the mosaic by one variable, arrow down
  increases the complexity by one, if possible. Arrow left
  and right rotate a previously included variable into the
  last split position. Conditioning/Unconditioning is done
  with keys 'C' and 'U' Keys 'B' and 'S" switch to bar and
  spine representation, respectively Key 'R' rotates the
  last split variable between horizontal and vertical
  display.
}
\examples{
library(cranvas)
library(productplots)
library(plyr)


## Simpson on board the Titanic?!
data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Passenger <- titanic$Class != "Crew"
qtitanic <- qdata(titanic, color = Survived)

qtitanic$.brushed <- qtitanic$Survived == "Yes"
## passengers have a higher probability of survival than crew members ...
print(qmosaic(qtitanic, Freq ~ Passenger, divider = c("hspine")))
## unless sex is taken into account: the conditional association between
## Passenger and Survival changes to the opposite: both male and female crew
## members had better changes of survival than their counterparts on the
## passenger side.
print(qmosaic(qtitanic, Freq ~ Passenger | Sex, divider = c("hspine", "hspine")))

## this is an example of the Simpson's paradox, and is due to the strong
## association between crew membership and gender.
print(qmosaic(qtitanic, Freq ~ Sex + Passenger, divider = c("vspine", "hspine")))

#######################
data(happy)
qhappy <- qdata(happy, color = happy)

qmosaic(qhappy, ~sex + marital, divider = c("vspine", "hspine"))
qmosaic(qhappy, ~marital + happy + year, divider = c("vspine", "hspine", "hspine"))
qmosaic(qhappy, ~marital + degree, divider = c("vspine", "hspine"))
qmosaic(qhappy, ~happy, divider = c("hbar"))

qbar(sex, qhappy)


print(qmosaic(qtitanic, Freq ~ Sex + Age + Class))
print(qbar(Survived, data = qtitanic))

print(qmosaic(qtitanic, Freq ~ Survived, "hbar"))
print(qmosaic(qtitanic, Freq ~ Age, "hbar"))
print(qmosaic(qtitanic, Freq ~ Sex, "hbar"))
print(qmosaic(qtitanic, Freq ~ Class, "hbar"))

print(qmosaic(qtitanic, Freq ~ Sex + Survived | Class, divider = c("vspine", "hspine", 
    "hspine")))
data(happy)
qhappy <- qdata(happy)
qmosaic(qhappy, ~happy, "hbar")
qmosaic(qhappy, ~happy + age, c("hspine", "vspine"))
}
\author{
  Heike Hofmann
}
