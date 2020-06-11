library("ISLR")
View(College)

#划分前600个样本为training test，其余177个为testing set
train=(0:600)
training_set = College[train,]
test=(-train)
testing_set = College[(-train),]

#subset selection(exhaustive search)
library(leaps)
regfit.full=regsubsets(log(Apps)~.,data=training_set,nvmax = 17,method = "exhaustive")
summary(regfit.full)

plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="bic")
plot(regfit.full,scale="Cp")

#subset selection using test-validation
train_cv=sample(1:nrow(training_set),nrow(training_set)*3/4)
regfit.best=regsubsets(log(Apps)~.,data=training_set[train_cv,],nvmax=17)
test.mat=model.matrix(log(Apps)~.,data=training_set[-train_cv,])
val.errors=rep(NA,17)

for(i in 1:17){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((log(training_set$Apps)[-train_cv]-pred)^2)
}

plot(1:17,val.errors,xlab="Number of Predictors",
     ylab="Validation Set Error",type='b')
which.min(val.errors)

coefi = coef(regfit.full,which.min(val.errors))
test.mat=model.matrix(log(Apps)~.,data=testing_set)
pred=test.mat[,names(coefi)]%*%coefi
mean((log(testing_set$Apps)-pred)^2)

#subset selection using 10-fold cross-validation
library(leaps)

predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  coefi = coef(object, id)
  test.mat = model.matrix(form, newdata)
  pred = test.mat[ , names(coefi)]%*%coefi
  return(pred)
}

k=10
folds=sample(1:k,nrow(training_set),replace=TRUE)
out.cv = matrix(0, nrow = k, ncol = 17)
colnames(out.cv)=1:17

for (j in 1:k) {
  best.fit=regsubsets(log(Apps)~.,data=training_set[folds!=j,],nvmax=17)
  for (i in 1:17) {
    pred = predict.regsubsets(best.fit, training_set[folds == j, ], i)
    MSE = mean((log(training_set$Apps)[folds == j] - pred)^2)
    out.cv[j, i] = MSE
  }
}
out.cv
cv.MSE = apply(out.cv, 2, mean)
plot(1:17,cv.MSE,xlab="Number of Predictors",
     ylab="10-fold Validation Set Error",type='b')

m = which.min(cv.MSE)
coef(regfit.full,m)

reg.full.train=regsubsets(log(Apps)~.,data=training_set,nvmax=17)
pred = predict.regsubsets(reg.full.train,newdata = testing_set,id=m)
mean((pred-log(testing_set$Apps))^2)


#shrinkage method (Lasso regression)
gridLambda=10^seq(5,-2,length=100)
train_x=model.matrix(log(Apps)~.,training_set)
test_x=model.matrix(log(Apps)~.,testing_set)
train_y=log(training_set$Apps)

library(glmnet)
lasso.mod=cv.glmnet(train_x,train_y,alpha=1,lambda=gridLambda)

#给定最佳的lambda值，算coefficient
bestlam=lasso.mod$lambda.min
predict(lasso.mod,s=bestlam,type="coefficients")

#算test MSE
lasso.pred=predict(lasso.mod,s=bestlam,newx=test_x)
mean((lasso.pred-log(testing_set$Apps))^2)


#降维 PCR
library(pls)
set.seed(1)
pcr.fit <- pcr(log(Apps)~.,data=training_set,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP",ylim= c(0.2,0.4))

#PCR的test MSE
pcr.fit11=pcr(log(Apps)~.,data=training_set,scale=TRUE)
pcr.pred=predict(pcr.fit11,testing_set,ncomp=15)
mean((pcr.pred-log(testing_set$Apps))^2)

#降维 PLSR
pls.fit=plsr(log(Apps)~., data=training_set,scale=TRUE,
             validation="CV")
validationplot(pls.fit,val.type="MSEP",ylim= c(0.25,0.4))

#PLSR的test MSE
pls.fit11=plsr(log(Apps)~.,data=training_set,scale=TRUE)
pls.pred=predict(pls.fit11,testing_set,ncomp=4)
mean((pls.pred-log(testing_set$Apps))^2)




