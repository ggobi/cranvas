library(cranvas)
data(tennis)
## the first step is always to create the data object
qtennis = qdata(tennis, color = Serve.Speed, fill = Serve.Speed, size = Aces)

## then create plots
qhist(Serve.Speed, data = qtennis)

qbar(Country)

qparallel(~.)  # all variables; you may need a large window

record_selector("Name")  # a simple GUI
