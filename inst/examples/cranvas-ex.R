library(cranvas)
str(tennis)

## the first step is always to create the data object
qtennis = qdata(tennis, color = serve.speed, size = aces)

## then create plots
qhist(serve.speed, data = qtennis)

qbar(country, data = qtennis)

qscatter(first.serve.pts, second.serve.pts, data = qtennis)

qboxplot(~points+first.serve.pts+second.serve.pts, data = qtennis)

qparallel(~., data = qtennis, names = names(tennis), horizontal = TRUE)  # all variables; you may need a large window

record_selector(name, data = qtennis)  # a simple GUI to show the names
