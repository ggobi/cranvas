## examples of qhist()
require(ggplot2)
#require(stringr)

#require(qtbase)
#require(qtpaint)
#require(plumbr)
require(cranvas)

data(tennis)
qtennis <- qdata(tennis)
print(qhist("First.Serve.Pct", qtennis, horizontal = FALSE))
print(qhist("Serve.Speed", qtennis, horizontal = FALSE))

