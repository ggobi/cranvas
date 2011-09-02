library(cranvas)
data(tennis)
str(tennis)

## the first step is always to create the data object
qtennis = qdata(tennis, color = serve.speed, size = aces)

## then create plots
qhist(serve.speed, data = qtennis)

qbar(country)

qparallel(~.)  # all variables; you may need a large window

record_selector(name)  # a simple GUI to show the names
