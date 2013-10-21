library(cranvas)
str(tennis)

## the first step is always to create the data object
qtennis = qdata(tennis, color = serve.speed, size = aces)

## then create plots
qhist(serve.speed, data = qtennis)

qbar(country, data = qtennis)

qscatter(first.serve.pts, second.serve.pts, data = qtennis)

qboxplot(~points+first.serve.pts+second.serve.pts, data = qtennis)

# all variables; you may need a large window
qparallel(~., data = qtennis, names = names(tennis), horizontal = TRUE)

record_selector(name, data = qtennis)  # a simple GUI to show the names

cranvas_off()
