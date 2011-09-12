library(cranvas)

### (1) simple maps
## the world map
qworld = map_qdata('world')
qmap2(qworld)

## US states
qstate = map_qdata('state')
qmap2(qstate)

## a simple demonstration of colors
qstate$.color = sample(c("red", "blue"), nrow(qstate), replace=TRUE)

## US counties
qcounty = map_qdata('county')
qmap2(qcounty)

## map of China
if (require('mapdata')) {
    qchina = map_qdata('china')
    qmap2(qchina)  # oh my! obviously china map needs more love
}



### (2) maps linked to other datasets
data(crimes)
head(crimes)
qcrimes = qdata(crimes, color = population)  # population mapped to colors

qparallel(names(crimes)[-c(1, 2)], data = qcrimes)  # par-coords without first 2 columns
qscatter(population, robbery, data = qcrimes)

## categorical linking by states; now brush on any plot and see changes in other plots
qmap2(qstate, linkto = qcrimes, linkby = 'state')

## linking should be automatically removed when map is closed; if not, try remove_link(qcrimes); remove_link(qstate)
