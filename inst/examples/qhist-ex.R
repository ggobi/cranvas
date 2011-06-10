## examples of qhist()
require(ggplot2)
require(cranvas)
require(scales)

data(tennis)
qtennis <- qdata(tennis)

# Not working yet
#qtennis <- qdata(tennis, fill=dscale(factor(tennis$Matches), hue_pal()))

print(qhist(First.Serve.Pct, qtennis, horizontal = FALSE))
print(qhist(Serve.Speed, qtennis, horizontal = FALSE))

print(qhist(First.Serve.Pct, qtennis, splitByCol="Matches",horizontal = FALSE))

qtennis <- qdata(tennis, fill=dscale(factor(tennis$Matches), hue_pal()))

# Position argument doesn't seem to work.
print(qhist(First.Serve.Pct, qtennis, splitByCol="Matches",horizontal = FALSE, postion="dodge"))
print(qhist(First.Serve.Pct, qtennis, splitByCol="Matches",horizontal = FALSE, postion="relative"))
print(qhist(First.Serve.Pct, qtennis, splitByCol="Matches",horizontal = FALSE, postion="identity"))

