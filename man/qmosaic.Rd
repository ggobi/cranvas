\name{qmosaic}
\alias{qmosaic}
\title{Mosaic plot.}
\usage{qmosaic(data, formula, divider=mosaic(), cascade=0, scale_max=TRUE,
    na.rm=FALSE, subset, main, cache=T, ...)
}
\description{Mosaic plot.
Create a mosaicplot using a formula (as described in prodplot)}
\details{up arrow - takes out top var,
down arrow - adds next variable (if available)
r toggles top variable between horizontal and vertical direction
b/s changes top variable between (b)archarts and (s)pine plots
u/c (un)conditions on the bottom variable}
\value{NULL}
\author{Heike Hofmann}
\arguments{\item{data}{a mutaframe which is typically built upon a data frame
along with several row attributes}
\item{formula}{a formula to describe order in which variables go into the mosaicplot. The first variables are the ones visually most important, i.e. Y ~ X1 + X2 + X3 first splits according to X3, then according to X2, then according to X1}
\item{divider}{structure of the split in each direction. Choices are 'hbar', 'vbar' for horizontal/vertical barcharts, 'hspine', 'vspine' for horizontal/vertical spineplots.}
\item{cascade}{parameter for prodplot in package productplots}
\item{scale_max}{parameter for prodplot in package productplots}
\item{na.rm}{handling of missing values, defaults to FALSE}
\item{subset}{parameter for prodplot -}
\item{main}{parameter for prodplot}
\item{cache}{boolean to turn cache on for layers, defaults to TRUE}
\item{...}{}
}
\examples{require(qtbase)
require(qtpaint)
require(cranvas)


require(stringr)
require(productplots)
require(plumbr)

data(Titanic)
titanic <- as.data.frame(Titanic)
qtitanic <- qdata(titanic)

print(qmosaic(qtitanic, Freq ~ Survived, "hbar")) 
print(qmosaic(qtitanic, Freq ~ Age, "hbar")) 
print(qmosaic(qtitanic, Freq ~ Sex, "hbar")) 
print(qmosaic(qtitanic, Freq ~ Class, "hbar")) 


print(qmosaic(qtitanic, Freq ~ Sex + Age | Class, mosaic()))}

