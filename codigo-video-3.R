par(mfrow=c(2,3),
    mar=c(1,1,1.5,1),
    oma=c(1,1,1,1),
    bg = 'lightgray')
z <- rnorm(1000)
plot(z,main='',xlab = '',col=2,las = 1)#dispersión
box()
hist(z,main='',xlab = '',col=3)
box()
qqnorm(z,main='',xlab = '',col=4)
qqline(z,col=6,lwd=2)
box()
boxplot(z,xlab = '',col=5,las = 1)
box()
plot.ecdf(z,main='',xlab = '',col=9,lwd=1.5)
box()
curve(dnorm(x),-3,3,xlab = '',col='orange',lwd=3)
box()
mtext('Los graficos de la N(0,1)',
      side=3,line=-1,font=2,outer = T)

#############################################
z <- rexp(1000)
plot(z,main='',xlab = '',col=2,las = 1)
box()
hist(z,main='',xlab = '',col=3)
box()
qqnorm(z,main='',xlab = '',col=4)
qqline(z,col=6,lwd=2)
box()
boxplot(z,xlab = '',col=5,las = 1)
box()
plot.ecdf(z,main='',xlab = '',col=9,lwd=1.5)
box()
curve(dexp(x),0,3,xlab = '',col='orange',lwd=3)
box()
mtext('Los graficos de la Exp(1)',
      side=3,line=-1,font=2,outer = T)

#############################################
z <- rgeom(1000,0.5)
plot(z,main='',xlab = '',col=2,las = 1)
box()
hist(z,main='',xlab = '',col=3)
box()
qqnorm(z,main='',xlab = '',col=4)
qqline(z,col=7,lwd=2)
box()
boxplot(z,xlab = '',col=5,las = 1)
box()
plot.ecdf(z,main='',xlab = '',col=9,lwd=1.5)
box()
plot(0:8,dgeom(0:8,0.5),xlab = '',ylab = '',type = 'h',col='magenta',lwd=3)
box()
mtext('Los graficos de la Geo(1/2)',
      side=3,line=-1,font=2,outer = T)

#############################################
z <- rbinom(1000,100,1/2)
plot(z,main='',xlab = '',col=2,las = 1)
box()
hist(z,main='',xlab = '',col=3)
box()
qqnorm(z,main='',xlab = '',col=4)
qqline(z,col=7,lwd=2)
box()
boxplot(z,xlab = '',col=5,las = 1)
box()
plot.ecdf(z,main='',xlab = '',col=9,lwd=1.5)
box()
plot(30:70,dbinom(30:70,100,0.5),xlab = '',ylab = '',type = 'h',col='magenta',lwd=3)
curve(dnorm(x,100*1/2,sqrt(100*1/2*1/2)),add=T,lwd=2)
box()
mtext('Los graficos de la Bin(100,1/2)',
      side=3,line=-1,font=2,outer = T)


