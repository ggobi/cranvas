## Looking at the NRC rankings data with cranvas
## Indicators are sorted by ISU, most above median to most below
## Use the university selector to choose ISU, examine its strengths/weaknesses
## Hold CTL and then select NCSU, to compare the two
library(cranvas)

# scatterplots of ratings variables, 5th vs 95th percentiles
# parallel coords of criteria
# lookup text window

qnrc = qdata(nrcstat)
rownames(qnrc) = paste(nrcstat$Institution, nrcstat$ProgramName, 
    sep = " -> ")
qnrc$.color = "red"
qnrc$.border = "red"

record_selector(Institution, qnrc)

qscatter(RRankings95th, RRankings5th, data = qnrc)
qscatter(SRankings95th, SRankings5th, data = qnrc)

median.centering <- function(x) {
    x <- (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
    x <- x - median(x, na.rm = T)
}
nrc.medctr <- apply(nrcstat[, 20:72], 2, median.centering)
var.ord <- order(nrc.medctr[13, ]) + 19
qparallel(qnrc, vars = var.ord, main = "Other Indicators", center = median, 
    horizontal = TRUE, glyph = "tick", boxplot = TRUE)
 
