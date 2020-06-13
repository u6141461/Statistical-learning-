library(ISLR)
attach(Wage)
View(Wage)

#划分训练集和测试集
train = sample(1:nrow(Wage),nrow(Wage)*9/10)
train_set = Wage[train,]
test_set = Wage[-(train),]
plot(age,wage,data =train_set)

#____________________________________________________
#多项式回归
fit=lm(wage~poly(age,4,raw=T),data=train_set)

#10-cv to choose tuning parameter(polynomial degree)
k=10
folds=sample(1:k,nrow(train_set),replace=TRUE)
out.cv = matrix(0, nrow = k, ncol = 8)
colnames(out.cv)=1:8

for (j in 1:k) {
    for (i in 1:8) {
    fit=lm(wage~poly(age,i,raw=T),data=train_set[folds!=j,])
    pred = predict(fit, train_set[folds == j, ])
    MSE = mean((train_set$wage[folds == j] - pred)^2)
    out.cv[j, i] = MSE
  }
}
out.cv
cv.MSE = apply(out.cv, 2, mean)
plot(1:8,cv.MSE,xlab="Number of Predictors",
     ylab="10-fold Validation Set Error",type='b')
which.min(cv.MSE)

#anova method to choose tuning parameter
fit.1=lm(wage~age,data=train_set)
fit.2=lm(wage~poly(age,2,raw=T),data=train_set)
fit.3=lm(wage~poly(age,3,raw=T),data=train_set)
fit.4=lm(wage~poly(age,4,raw=T),data=train_set)
fit.5=lm(wage~poly(age,5,raw=T),data=train_set)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

#calculate test MSE
pred_y = predict(fit.4,test_set)
mean((test_set$wage - pred_y)^2)

#visualize the polynomial regression
plot(age,wage,data =train_set)

agelims=range(age)
agelims
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit.4,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit
               -2*preds$se.fit)

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# _____________________________________________
#logistics多项式回归
fit=glm(I(wage>250)~poly(age,9),data=train_set,family=binomial)

#10-fold cv to choose tuning parameter
cv.error = rep(0,10)
for( i in 1:10 ){ 
  glm.fit = glm( wage ~ poly(age,i), data=train_set )
  cv.error[i] = cv.glm( train_set, glm.fit, K=10 )$delta[1]
}
which.min(cv.error)

#calculate test missclassification error rate
glm.probs = predict(fit, test_set,"response")
glm.pred <- ifelse(glm.probs>.5, TRUE,FALSE)
mean(I(test_set$wage>250)!=glm.pred)

#visualize
preds=predict(fit,newdata=list(age=age.grid),se=T)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


#_______________________________________
#step function
table(cut(age,4))
fit=lm(wage~cut(age,3),data=train_set)

#calculate test MSE
pred = predict(fit,data=test_set)
mean((pred-test_set$wage)^2)

# visualize step function 
preds=predict(fit,newdata=list(age=age.grid),se=T)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit
               -2*preds$se.fit)
plot(age,wage,xlim=agelims,data = train_set,cex=.5,col="darkgrey")
title("stepfunction",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

#K-fold cv to choose optimal cut (cv.glm 没法用cut)
number_of_bins = c( 2, 3, 4, 5, 10 )
nc = length(number_of_bins)

k = 10
folds = sample( 1:k, nrow(train_set), replace=TRUE ) 
cv.errors = matrix( NA, k, nc )

age_range = range( train_set$age )
age_range[1] = age_range[1]-1
age_range[2] = age_range[2]+1

for( ci in 1:nc ){ # for each number of cuts to test
  nob = number_of_bins[ci] # n(umber) o(f) c(uts) 2, 3, 4 ...
  for( fi in 1:k ){ # for each fold
    fit = glm( wage ~ cut( age, breaks=seq( from=age_range[1], to=age_range[2], length.out=(nob+1) ) ), data=train_set[folds!=fi,] )
    y_hat = predict( fit, newdata=train_set[folds==fi,] )
    cv.errors[fi,ci] = mean( ( train_set[folds==fi,]$wage - y_hat )^2 ) 
  }
}
cv.errors.mean = apply(cv.errors,2,mean)
cv.errors.stderr = apply(cv.errors,2,sd)/sqrt(k)

min.cv.index = which.min( cv.errors.mean )
one_se_up_value = ( cv.errors.mean+cv.errors.stderr )[min.cv.index] 

min_lim=min( one_se_up_value, cv.errors.mean, cv.errors.mean-cv.errors.stderr, cv.errors.mean+cv.errors.stderr ) * 0.9
max_lim=max( one_se_up_value, cv.errors.mean, cv.errors.mean-cv.errors.stderr, cv.errors.mean+cv.errors.stderr ) * 1.1

plot( number_of_bins, cv.errors.mean, ylim=c(min_lim,max_lim), pch=19, type='b', xlab='number of cut bins', ylab='CV estimate of the prediction error' )
lines( number_of_bins, cv.errors.mean-cv.errors.stderr, lty='dashed' )
lines( number_of_bins, cv.errors.mean+cv.errors.stderr, lty='dashed' )
abline( h=one_se_up_value, col='red' )
grid()

#____________________________________
#spline
library(splines)
fit=lm(wage~ns(age,knots=c(25,40,60)),data=train_set)
# dim(bs(age,knots=c(25,40,60)))
# dim(bs(age ,df=6))
fit=lm(wage~ns(age,df=6),data=train_set)


#visualize the splines regression
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray",data=train_set)
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

#smoothing spline
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),
       lty=1,lwd=2,cex=.8)

#local regression
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=train_set)
fit2=loess(wage~age,span=.5,data=train_set)

agelims=range(age)
agelims
age.grid=seq(from=agelims[1],to=agelims[2])

lines(age.grid,predict(fit,data.frame(age=age.grid)),
      col="red",lwd=2)  
lines(age.grid,predict(fit2,data.frame(age=age.grid)),
      col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),
       col=c("red","blue"), lty=1,lwd=2,cex=.8)

#calculate test MSE
mean((test_set$wage-predict(fit2,data = test_set))^2)
