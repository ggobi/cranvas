library(cranvas)
## tennis data
data(tennis)

qtennis = qdata(tennis)
qbar(matches)

## NRC rankings
data(nrcstat)

qnrc = qdata(nrcstat, color = Control)  # Control: public or private univ
qbar(RegCode, main = 'Number of public and private colleges in each region')

qparallel(vars = 13:10, main = "Overview of Rankings", glyph = "tick",
    horizontal = TRUE, boxplot = TRUE)

qbar(RegCode, horizontal = TRUE)  # horizontal plot
qbar(RegCode, standardize = TRUE)  # standardize to 1

## use border color to split the bars
qnrc2 = qdata(nrcstat, color = 'white', border = Control)
qbar(RegCode)
