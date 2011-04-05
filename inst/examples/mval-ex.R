library(cranvas)

source("mval.R")


brfss <- read.csv("files/Heike/brfss09std.csv")
qbrfss <- qdata(brfss)
qmval(qbrfss, vars = 1:50)
qparallel(qbrfss, vars = 1:3)

library(cranvas)
source("mval.R")
tao <- read.csv("../files/Heike/tao.csv")
qtao <- qdata(tao)

qmval(qtao) 
