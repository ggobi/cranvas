## Looking at the NRC rankings data with cranvas
## Indicators are sorted by ISU, most above median to most below
## Use the university selector to choose ISU, examine its strengths/weaknesses
## Hold CTL and then select NCSU, to compare the two

require(qtbase)
require(qtpaint)
require(plumbr)
require(cranvas)

# scatterplots of ratings variables, 5th vs 95th percentiles
# parallel coords of criteria
# lookup text window
data(nrcstat)
nrcstat[, 26] <- -nrcstat[, 26]
colnames(nrcstat)[26] <- "Neg.Median.Time.to.Degree"

qnrc = qdata(nrcstat)
rownames(qnrc) = paste(nrcstat$Institution.Name, nrcstat$Program.Name, 
    sep = " -> ")
qnrc$.color = "red"

record_selector(qnrc, "Institution.Name")

qscatter(x = R.Rankings.95th.Percentile, y = R.Rankings.5th.Percentile, data = qnrc)
qscatter(S.Rankings.95th.Percentile, S.Rankings.5th.Percentile, qnrc)

median.centering <- function(x) {
    x <- (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
    x <- x - median(x, na.rm = T)
}
nrc.medctr <- apply(nrcstat[, 20:68], 2, median.centering)
var.ord <- order(nrc.medctr[13, ]) + 19
qparallel(qnrc, vars = var.ord, main = "Other Indicators", center = median, 
    horizontal = FALSE, glyph = "tick", boxplot = TRUE, boxwex = 0.8)
var.ord.sub <- var.ord[-c(1, 6, 7, 10, 17, 19, 32:35, 39, 40, 41, 43:45, 
    47:49)]
qparallel(qnrc, vars = var.ord.sub, main = "Other Indicators", center = median, 
    horizontal = FALSE, glyph = "tick", boxplot = TRUE, boxwex = 0.6)
 
