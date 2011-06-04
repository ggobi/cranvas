library(cranvas)

df = matrix(rnorm(1000), 200)
colnames(df) <- c("V1", "V2", "V3", "V4", "V5")
qboxplot(df)
qboxplot(df, horizontal = TRUE)
qboxplot(df, at = (1:5)^2)  # at different locations
qboxplot(df, width = .1*sample(5))  # different widths
qboxplot(rnorm(100))
qboxplot(Sepal.Length~Species,data=iris)

df = qdata(round(data.frame(x = rnorm(100), y = runif(100), z = rbeta(100, 1, .8)), 2))

qboxplot(data = df)
qparallel(~ ., data = df)  # brush on par-coords and you see little boxplots

data(flea)
qflea <- qdata(flea[,1:6])
qboxplot(. ~  species, data = flea)
