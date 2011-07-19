## examples of qhist()
library(ggplot2)
library(cranvas)
library(scales)

data(tennis)
qtennis <- qdata(tennis)

# Not working yet
#qtennis <- qdata(tennis, fill=dscale(factor(tennis$Matches), hue_pal()))

print(qhist(First.Serve.Pct, qtennis, horizontal = FALSE))
print(qhist(Serve.Speed, qtennis, horizontal = FALSE))

print(qhist(First.Serve.Pct, qtennis, splitByCol="Matches",horizontal = FALSE))
print(qhist(Serve.Speed, qtennis, splitByCol="Matches",horizontal = FALSE))

# Position argument doesn't seem to work.
print(qhist(First.Serve.Pct, qtennis, splitByCol="Matches",horizontal = FALSE, postion="dodge"))
print(qhist(First.Serve.Pct, qtennis, splitByCol="Matches",horizontal = FALSE, postion="relative"))
print(qhist(First.Serve.Pct, qtennis, splitByCol="Matches",horizontal = FALSE, postion="identity"))

# categorical variable linking
data(flea)
qflea <- qdata(flea)
print(qhist(tars1, qflea))
print(qhist(aede1, qflea))

print(qhist(tars1, qflea, splitByCol="species"))
print(qhist(aede1, qflea, splitByCol="species"))

link_var(qflea) = 'species'
link_type(qflea) = 'self'
