## examples of qhist()
require(ggplot2)
require(cranvas)

data(tennis)
qtennis <- qdata(tennis)
print(qhist("First.Serve.Pct", qtennis, horizontal = FALSE))
print(qhist("Serve.Speed", qtennis, horizontal = FALSE))

print(qhist("First.Serve.Pct", qtennis, splitByCol="Matches",horizontal = FALSE))
# Position argument doesn't seem to work.
print(qhist("First.Serve.Pct", qtennis, splitByCol="Matches",horizontal = FALSE, postion="dodge"))
print(qhist("First.Serve.Pct", qtennis, splitByCol="Matches",horizontal = FALSE, postion="relative"))
print(qhist("First.Serve.Pct", qtennis, splitByCol="Matches",horizontal = FALSE, postion="identity"))

