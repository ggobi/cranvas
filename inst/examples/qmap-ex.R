library(cranvas)

### (1) simple maps
## the world map
qworld = map_qdata('world')
qmap(qworld)

## US states
qstate = map_qdata('state')
qmap(qstate)

## a simple demonstration of colors
qstate$.color = sample(c("red", "blue"), nrow(qstate), replace=TRUE)

## US counties
qcounty = map_qdata('county')
qmap(qcounty)

## map of China
if (require('mapdata')) {
    qchina = map_qdata('china')
    qmap(qchina)  # oh my! obviously china map needs more love
}



### (2) maps linked to other datasets
data(crimes)
head(crimes)
qcrimes = qdata(crimes, color = population)  # population mapped to colors
qcrimes = qdata(crimes, color = population, low = "#FCBBA1", high = "red")  # change the color scheme from http://colorbrewer2.org/

qparallel(names(crimes)[-c(1, 2)], data = qcrimes)  # par-coords without first 2 columns
qscatter(population, robbery, data = qcrimes)

## categorical linking by states; now brush on any plot and see changes in other plots
qmap(qstate, linkto = qcrimes, linkby = 'state')

## linking should be automatically removed when map is closed; if not, try remove_link(qcrimes); remove_link(qstate)



### (3) cartogram (presidential election results)

vote.res = c("red", "red", "red", "blue", "blue", "blue", "blue",
  "blue", "blue", "red", "red", "blue", "blue", "blue", "red", "red",
  "red", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
  "blue", "red", "red", "red", "red", "blue", "blue", "blue", "blue",
  "blue", "blue", "blue", "blue", "blue", "blue", "blue", "red",
  "blue", "red", "blue", "blue", "blue", "red", "red", "red", "red",
  "red", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
  "blue", "blue", "red", "blue", "red")

qstate = map_qdata('state', color = vote.res)
qmap(qstate)  # a normal map

## cartogram based on population

vote.pop = c(4708708, 6595778, 2889450, 36961664, 5024748, 3518288,
  885122, 601723, 18537969, 9829211, 1545801, 12910409, 6423113,
  3007856, 2818747, 4314113, 4492076, 1318301, 5699478, 6593587,
  6593587, 6593587, 9969727, 9969727, 5266214, 2951996, 5987580,
  974989, 1796619, 2643085, 1324575, 8707739, 2009671, 19541453,
  19541453, 19541453, 19541453, 9380884, 9380884, 9380884, 646844,
  11542645, 3687050, 3825657, 12604767, 1053209, 4561242, 812383,
  6296254, 24782302, 2784572, 621760, 7882590, 7882590, 7882590,
  6664195, 6664195, 6664195, 6664195, 6664195, 1819777, 5654774,
  544270)

qstate2 = map_qdata('state', color = vote.res, size = vote.pop, cartogram = TRUE)
qmap(qstate2)

## electoral votes for each state
vote.num = c(9, 10, 6, 55, 9, 7, 3, 3, 27, 15, 4, 21, 11, 7, 6, 8, 9,
  2, 10, 12, 12, 12, 17, 17, 10, 6, 11, 3, 2, 5, 4, 15, 5, 31, 31, 31,
  31, 15, 15, 15, 3, 20, 7, 7, 21, 4, 8, 3, 11, 34, 5, 3, 13, 13, 13,
  11, 11, 11, 11, 11, 5, 10, 3)

qstate3 = map_qdata('state', color = vote.res, size = vote.num, cartogram = TRUE)
qmap(qstate3)
