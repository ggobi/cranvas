library(cranvas)
data(tennis)
tennis$Matches = factor(tennis$Matches)
qtennis = qdata(tennis)

qhist(First.Serve.Pct, qtennis)
qbar(Matches, qtennis)

qhist(First.Serve.Pct, qtennis, horizontal = TRUE)

qhist(Serve.Speed, qtennis)
qhist(Serve.Speed, qtennis, freq = FALSE)  # density

## categorical variable linking
data(flea)
qflea <- qdata(flea)
qhist(tars1, qflea)
qhist(aede1, qflea)

link_var(qflea) = 'species'
link_type(qflea) = 'self'
## now brush one bar, all rows in the same species will be brushed

## map tars1 to colors
qflea2 <- qdata(flea, color = tars1)
qhist(tars1, qflea2)
