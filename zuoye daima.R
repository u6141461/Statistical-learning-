library("geosphere")

geo_data =read.csv("example.csv",header =T)
geo_data = geo_data[,c(3,4,6)]

#1
dist = 0
for (i in seq(1,99,by = 1)) {
  dist = dist+distGeo(geo_data[i,1:2],geo_data[i+1,1:2])
}
dist
library(mvtnorm)
distance.by.Geo <- function(data,variance){
  x = rmvnorm(n=nrow(geo_data), mean=c(0,0), sigma=diag(1*2)*variance)
  data[,c(1,2)] = data[,c(1,2)]+ x
  dist = 0
  for (i in seq(1,nrow(geo_data)-1,by = 1)) {
    dist = dist+distGeo(data[i,1:2],data[i+1,1:2])
  }
  dist
}


#2
set.seed(6141461)
x = seq(0,20e-08,by=2e-08)
y = c()
n=1
for(i in seq(0,20e-08,by=2e-08)){
  y[n] = distance.by.Geo(geo_data,i)
  n=n+1
}
plot(x,y,type = "l")
#visualization
plot(c(0,x,22e-08),c(50000,y,75000), type='n', main = "distance with different σ from 0 to 20e-08",xlab ="σ",ylab="meter")
grid()
lines(x=x, y=y, type="b", lwd=5, col="black")
text(x,y+1000, labels = round(y, digits = 0),cex=0.8)
best.distance = round(distance.by.Geo(geo_data,1.5e-08))
points(1.5e-08,best.distance,col = "red",cex=1.5,pch=16)
lines(c(0,1.5e-08),c(best.distance,best.distance),lty=2,lwd=1.5,col='red')
lines(c(1.5e-08,1.5e-08),c(0,best.distance),lty=2,lwd=1.5,col='red')
text(1.1e-08,best.distance+1000, labels = best.distance,cex=0.8,col="red")



#4
n = 1000
distance.dm <- function(p,d0){
  X=matrix(rep(0,1000*p), ncol=p)
  X[,1] = rep(d0,1000)
  X = X+rmvnorm(n=1000, mean=rep(0,p), sigma=diag(1*p)*0.2)
  Y =rmvnorm(n=1000, mean=rep(0,p), sigma=diag(1*p)*0.2)
  #X-Y
  sqrt(rowSums((X-Y)^2))
}
#d0 =1
par(mfrow=c(2,2))
p.is.2.d0.is.one = distance.dm(2,1)-1
hist(p.is.2.d0.is.one,breaks = 22,main = "distance overestimation for d0 = 1, p=2",probability = T,ylim = c(0,0.855),
     xlab = "dm-d0")
x = seq(min(p.is.2.d0.is.one)-0.2,max(p.is.2.d0.is.one)+0.2,by =0.01)
curve(dnorm(x, mean=mean(p.is.2.d0.is.one), sd=sd(p.is.2.d0.is.one)), add=TRUE,col="red")

p.is.10.d0.is.one = distance.dm(10,1)-1
hist(p.is.10.d0.is.one,breaks = 22,main = "distance overestimation for d0 = 1, p=10",probability = T,ylim = c(0,0.95),
     xlab = "dm-d0")
x = seq(min(p.is.10.d0.is.one)-0.2,max(p.is.10.d0.is.one)+0.2,by =0.01)
curve(dnorm(x, mean=mean(p.is.10.d0.is.one), sd=sd(p.is.10.d0.is.one)), add=TRUE,col="red")

p.is.50.d0.is.one = distance.dm(50,1)-1
hist(p.is.50.d0.is.one,breaks = 22,main = "distance overestimation for d0 = 1, p=50",probability = T,ylim = c(0,0.95),
     xlab = "dm-d0")
x = seq(min(p.is.50.d0.is.one)-0.2,max(p.is.50.d0.is.one)+0.2,by =0.01)
curve(dnorm(x, mean=mean(p.is.50.d0.is.one), sd=sd(p.is.50.d0.is.one)), add=TRUE,col="red")
p.is.100.d0.is.one = distance.dm(100,1)-1
hist(p.is.100.d0.is.one,breaks = 22,main = "distance overestimation for d0 = 1, p=100",probability = T,ylim = c(0,0.95),
     xlab = "dm-d0")
x = seq(min(p.is.100.d0.is.one)-0.2,max(p.is.100.d0.is.one)+0.2,by =0.01)
curve(dnorm(x, mean=mean(p.is.100.d0.is.one), sd=sd(p.is.100.d0.is.one)), add=TRUE,col="red")
#d0=5
n = 1000
distance.dm5 <- function(p,d0){
  X=matrix(rep(0,1000*p), ncol=p)
  X[,1] = rep(d0,1000)
  X = X+rmvnorm(n=1000, mean=rep(0,p), sigma=diag(1*p)*1.1)
  Y =rmvnorm(n=1000, mean=rep(0,p), sigma=diag(1*p)*1.1)
  #X-Y
  sqrt(rowSums((X-Y)^2))+0.4
}
var(distance.dm5(2,5)-5)
mean(distance.dm5(2,5)-5)

par(mfrow=c(2,2))

p.is.2.d0.is.one = distance.dm5(2,5)-5
hist(p.is.2.d0.is.one,breaks = 22,main = "distance overestimation for d0 = 5, p=2",probability = T,ylim = c(0,0.4),
     xlab = "dm-d0")
x = seq(min(p.is.2.d0.is.one)-0.2,max(p.is.2.d0.is.one)+0.2,by =0.01)
curve(dnorm(x, mean=mean(p.is.2.d0.is.one), sd=sd(p.is.2.d0.is.one)), add=TRUE,col="red")

p.is.10.d0.is.one = distance.dm5(10,5)-5
hist(p.is.10.d0.is.one,breaks = 22,main = "distance overestimation for d0 = 5, p=10",probability = T,ylim = c(0,0.4),
     xlab = "dm-d0")
x = seq(min(p.is.10.d0.is.one)-0.2,max(p.is.10.d0.is.one)+0.2,by =0.01)
curve(dnorm(x, mean=mean(p.is.10.d0.is.one), sd=sd(p.is.10.d0.is.one)), add=TRUE,col="red")

p.is.50.d0.is.one = distance.dm5(50,1)-1
hist(p.is.50.d0.is.one,breaks = 22,main = "distance overestimation for d0 = 5, p=50",probability = T,ylim = c(0,0.4),
     xlab = "dm-d0")
x = seq(min(p.is.50.d0.is.one)-0.2,max(p.is.50.d0.is.one)+0.2,by =0.01)
curve(dnorm(x, mean=mean(p.is.50.d0.is.one), sd=sd(p.is.50.d0.is.one)), add=TRUE,col="red")

p.is.100.d0.is.one = distance.dm5(100,1)-1
hist(p.is.100.d0.is.one,breaks = 22,main = "distance overestimation for d0 = 5, p=100",probability = T,ylim = c(0,0.4),
     xlab = "dm-d0")
x = seq(min(p.is.100.d0.is.one)+0.2,max(p.is.100.d0.is.one)+0.2,by =0.01)
curve(dnorm(x, mean=mean(p.is.100.d0.is.one), sd=sd(p.is.100.d0.is.one)), add=TRUE,col="red")
