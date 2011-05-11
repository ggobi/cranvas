df = matrix(rnorm(1000), 200)
qboxplot(df)
qboxplot(df, horizontal = TRUE)
qboxplot(df, at = (1:5)^2)  # at different locations
qboxplot(df, width = .1*sample(5))  # different widths
qboxplot(rnorm(100))
qboxplot(Sepal.Length~Species,data=iris)

df = qdata(round(data.frame(x = rnorm(100), y = runif(100), z = rbeta(100, 1, .8)), 2))
## brush(df, 'size') = 1  # I'm unable to work with size > 1
qboxplot(data = df)
qparallel(data = df)
