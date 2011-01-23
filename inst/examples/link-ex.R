## link the original data with a frequency table
data1 = qmutaframe(iris)
tab2 = as.data.frame(table(iris$Species))
colnames(tab2) = c('type', 'freq')
data2 = qmutaframe(tab2)

data1
data2

## specify the linking variable respectively: link 'Species' with 'type'
link_var(data1) = 'Species'
link_var(data2) = 'type'

link(data1, data2)

## now we brush data1
focused(data1) = TRUE  # suppose data1 is on focus
data1$.brushed[1] = TRUE  # the first row is brushed
## check the changes: data2 has been updated
data1
data2

## and we move the mouse over data2
focused(data2) = TRUE
focused(data1) = FALSE  # data1 lost focus, of course
data2$.brushed[3] = TRUE  # we brush the category 'virginica'
data2
data1

data2$.brushed[1] = FALSE  # do not brush 'setosa'
data2
data1
