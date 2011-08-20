library(cranvas)

## example 1: NASA temperature data
data(nasa)
nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
qnasa <- qdata(nasa11)
print(qtime(TimeIndx,~ts,qnasa,shift=c(1,12)))
print(qtime(TimeIndx,~ts,qnasa,wrap=FALSE))
print(qtime(TimeIndx,~ts,qnasa,Year,shift=1))
print(qtime(TimeIndx,~ts,qnasa,Year,wrap=FALSE))
print(qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,shift=c(1,12)))
print(qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,wrap=FALSE))
print(qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,Year))
print(qtime(TimeIndx,~ts+ps_tovs+ca_med,qnasa,Year,wrap=FALSE))


## example 2: Remifentanil in the nlme package
require(nlme)
Rem <- qdata(Remifentanil[complete.cases(Remifentanil) &
                          Remifentanil$ID==1,])
Remi <- qdata(Remifentanil[complete.cases(Remifentanil),])
Remi$ID <- factor(Remi$ID)
print(qtime(Time,~conc,Rem))
print(qtime(Time,~conc,Remi,group=ID))
print(qtime(Time,~conc,Remi,group=ID,wrap=FALSE))

# for categorical brushing self-link dataset by ID:
link_var(Remi) <- "ID"   # ON
link_type(Remi) <- "self"

link_var(Remi) <- NULL  # OFF

                          
## example 3: Wages data
data(wages)
wage <- qdata(wages[wages$id<2000,1:3])
print(qtime(exper,~lnw,wage,group=id))

wage$id <- as.factor(wage$id)
link_var(wage) <- "id"   # ON
link_type(wage) <- "self"
link_var(wage) <- NULL  # OFF
