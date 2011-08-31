library(cranvas)
data(flea, package = 'tourr')

### (1) linking to oneself through a categorical variable
qflea = qdata(flea, color = species)
qhist(tars1)  # an ordinary histogram; try brushing

## now we link qflea to itself by species
id = link_cat(qflea, 'species')
## brush the plot and see what happens

remove_listener(qflea, id)  # remove this linking; back to normal linking again


### (2) link the original data with a frequency table
tab2 = as.data.frame(table(flea$species))
colnames(tab2) = c("type", "freq")
(qflea2 = qdata(tab2))
head(qflea)  # what the two datasets look like

## see how two different datasets can be linked through a common categorical variable
id = link_cat(qflea, var1 = 'species', qflea2, var2 = 'type')
qhist(tars1, data = qflea)
qbar(type, data = qflea2, standardize = TRUE)


## remove the linking on two datasets respectively
remove_listener(qflea, id[1])
remove_listener(qflea2, id[2])
