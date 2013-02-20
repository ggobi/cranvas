library(cranvas)

### (1) linking to between two tables, using common id variable

qwg = qdata(wages.demog)
#qscatter(ged, race, data=qwg)
qbar(race, data=qwg)
qbar(ged, data=qwg)
qhist(hgc, data=qwg)
     
qwages = qdata(wages)
qscatter(exper, lnw, data=qwages, alpha=0.5)

id = link_cat(qwages, var1 = 'id', qwg, var2 = 'id')

remove_link(qwages, id[1])
remove_link(qwg, id[2])

### (2) linking to oneself through a categorical variable
data(flea, package = 'tourr')

qflea = qdata(flea, color = species)
qhist(tars1)  # an ordinary histogram; try brushing

## now we link qflea to itself by species
id = link_cat(qflea, 'species')
## brush the plot and see what happens

remove_link(qflea, id)  # remove this linking; back to normal linking again

### (3) link the original data with a frequency table
tab2 = as.data.frame(table(flea$species))
colnames(tab2) = c("type", "freq")
(qflea2 = qdata(tab2))
head(qflea)  # what the two datasets look like

## see how two different datasets can be linked through a common categorical variable
id = link_cat(qflea, var1 = 'species', qflea2, var2 = 'type')
qhist(tars1, data = qflea)
qbar(type, data = qflea2, standardize = TRUE)

## remove the linking on two datasets respectively
remove_link(qflea, id[1])
remove_link(qflea2, id[2])
