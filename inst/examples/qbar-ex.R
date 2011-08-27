library(cranvas)
## tennis data
data(tennis)

qtennis = qdata(tennis)
qbar(Matches)

## NRC rankings
data(nrcstat)

qnrc = qdata(nrcstat, color = Control)  # Control: public or private univ
qbar(Regional.Code, main = 'Number of public and private colleges in each region')

qparallel(vars = 13:10, main = "Overview of Rankings", glyph = "tick",
    horizontal = FALSE, boxplot = TRUE)

qbar(Regional.Code, horizontal = TRUE)  # horizontal plot

## use border color to split the bars
qnrc2 = qdata(nrcstat, color = 'white', border = Control)
qbar(Regional.Code)
