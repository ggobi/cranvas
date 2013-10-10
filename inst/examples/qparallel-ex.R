library(cranvas)

### (1) flea data
data(flea, package = 'tourr')
qflea = qdata(flea, color = species)

## input variables
qparallel(~., data = qflea)  # all variables
qparallel(~aede1+aede2+aede3, data = qflea)  # 3 variables
qparallel(1:4, data = qflea)  # first 4 variables
# input variables by names
qparallel(c('head','aede1','tars2','aede3'), data = qflea)

## scaling
qparallel(~., data = qflea, main = 'scale columns individually to [0, 1]')
qparallel(~., data = qflea, scale = "I", main = 'unscaled data')
qparallel(~., data = qflea, scale = "var", main = 'mean 0 variance 1')
qparallel(~., data = qflea, scale = "global", main = 'scale globally to [0, 1]')

## centering by median; add boxplots to assist understanding
qparallel(~., data = qflea, center = median, boxplot = TRUE)

## ordering
qparallel(~., data = qflea, order = 'MDS')  # similar variables together
# color is covariate, order by ANOVA p-value
qparallel(~., data = qflea, order = 'ANOVA')
## we can use arrow keys to manually order the variables

## horizontal direction
qparallel(~., data = qflea, horizontal = TRUE)

## use glyphs instead of segments
qparallel(~., data = qflea, glyph = 'tick')
qparallel(~., data = qflea, glyph = 'circle')

## jittering
qparallel(~., data = qflea, jitter = 'species')

## categorical linking
id = link_cat(qflea, 'species')
## now brush on plots; will see all rows in the same category brushed

## stop categorical linking
remove_link(qflea, id)

### (2) NRC rankings
qnrc = qdata(nrcstat, color = RegCode)

## Overview: type, rankings
qparallel(13:10, data = qnrc, main = "Overview of Rankings", glyph = "tick",
    horizontal = TRUE, boxplot = TRUE)

## link to a droplist (institution names)
record_selector(Institution, qnrc)

## TODO: we need keyboard interactions here instead of command line
brush(qnrc, 'persistent') = TRUE  # begin persistent brushing
brush(qnrc, 'color') = 'brown'
## select other objects now
brush(qnrc, 'color') = 'green'
## again, select other objects
brush(qnrc, 'color') = 'yellow'
brush(qnrc, 'persistent') = FALSE  # transient brushing

qparallel(vars = 14:19, data = qnrc,
    main = "Research, Student Support, Diversity",
    center = median, horizontal = TRUE, glyph = "tick")


### (3) Missing values are imputed by 20% below the mean
df = as.data.frame(replicate(5, sample(c(rep(NA, 10), rnorm(100)))))
mf = qdata(df)
qparallel(~., data = mf)

## see this by missing value plot
qmval(~., data = mf)



### (4) alpha transparency
if (require('animation')) {
  data(pollen, package = 'animation')
  qpollen = qdata(pollen)
  qparallel(~.)  # hold the minus key (-) till the plot is semi-transparent
}



### (5) pressure test
test.mat = qdata(matrix(rnorm(300000 * 10), ncol = 10))
qparallel(~., data  = test.mat)
## for large data, short ticks are automatically used instead of segments
