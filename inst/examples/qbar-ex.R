library(cranvas)
## tennis data
data(tennis)

qtennis = qdata(tennis)
qbar(matches, data = qtennis)
qbar(matches, data = qtennis, weight = serve.speed)

## NRC rankings
data(nrcstat)

qnrc = qdata(nrcstat, color = Control)  # Control: public or private univ
qbar(RegCode, data = qnrc, main = 'Number of public and private colleges in each region')

qparallel(vars = 13:10, data = qnrc, main = "Overview of Rankings", glyph = "tick",
    horizontal = TRUE, boxplot = TRUE)

qbar(RegCode, data = qnrc, horizontal = TRUE)  # horizontal plot
qbar(RegCode, data = qnrc, standardize = TRUE)  # standardize to 1

## use border color to split the bars
qnrc2 = qdata(nrcstat, data = qnrc, color = 'white', border = Control)
qbar(RegCode, data = qnrc)
