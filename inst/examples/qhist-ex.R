## examples of qhist()
require(ggplot2)
require(cranvas)

data(tennis)
qtennis <- qdata(tennis)
print(qhist("First.Serve.Pct", qtennis, horizontal = FALSE))
print(qhist("Serve.Speed", qtennis, horizontal = FALSE))

print(qhist("First.Serve.Pct", qtennis, splitByCol="Matches",horizontal = FALSE))

data(flea)
qflea <- qdata(flea)
print(qhist("tars1", qflea, splitByCol="species",horizontal = FALSE))

