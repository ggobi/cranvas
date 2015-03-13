library(cranvas)

## a simple example: two polygons of size 1 and 5 respectively
theta=seq(0,2*pi,length=100)
x=c(cos(theta),NA,cos(theta)+2);y=c(sin(theta),NA,sin(theta)+1)

res=cart_polygon(x,y,c('x','y'),c(1,5),nrow=80,ncol=80,blank.init=0.4,sea.init=0.1,sea.width=0.5)

plot(res$x,res$y,type='n')
polygon(x,y); text(c(0,2),c(0,1),c(1,5))  # original polygons
polygon(res$x,res$y,border='red',lty=2,lwd=2)  # transformed polygons


## see map_qdata() for examples of real maps
