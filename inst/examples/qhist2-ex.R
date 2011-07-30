library(cranvas)
data(tennis)
tennis$Matches = factor(tennis$Matches)
qtennis = qdata(tennis, fill = Matches, color = Matches)

qhist2(First.Serve.Pct, qtennis)
qbar(Matches, qtennis)

qhist2(First.Serve.Pct, qtennis, horizontal = TRUE)

qhist2(Serve.Speed, qtennis)
qhist2(Serve.Speed, qtennis, freq = FALSE)  # density


## categorical variable linking
data(flea)
qflea <- qdata(flea)
qhist2(tars1, qflea)
qhist2(aede1, qflea)

link_var(qflea) = 'species'
link_type(qflea) = 'self'
## now brush one bar, all rows in the same species will be brushed

## map tars1 to colors
qflea2 <- qdata(flea, color = tars1, fill = tars1)
qhist2(tars1, qflea2)
