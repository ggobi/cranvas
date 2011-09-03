library(cranvas)

### (1) some test cases
df = qdata(as.data.frame(matrix(rnorm(1000), 200)))
qboxplot(~., df)
qboxplot(~., df, horizontal = TRUE)
qboxplot(~., df, at = (1:5)^2)  # at different locations
qboxplot(~., df, width = .1*sample(5))  # different widths


### (2) flea data
data(flea, package = 'tourr')
qflea <- qdata(flea)

## continuous vs categorical variable
qboxplot(tars1 ~ species, data = qflea)

## side-by-side boxplots for several continuous variables
qboxplot(~aede1+aede2+aede3, data = qflea)

## brush on other plots and we see little boxplots
qparallel(~ ., data = qflea)

qscatter(tars1, tars2, data = qflea)


## we can turn on the point layer too
qboxplot(tars1 ~ species, data = qflea, points = TRUE)

## show points with colors
qflea2 = qdata(flea, color = species)
qboxplot(~aede1+aede2+aede3, data = qflea2, points = TRUE)
