install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)
View(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

#______________________
#classification tree
#分为training set 和testing set
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]

#fit model
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

#calculate test Missclassification error rate
set.seed(2)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(34+15)/200
print(paste("The test MSE is ", (34+15)/200))

#using 10-cv 找best tuning parameter：size of tree 和 惩罚系数alpha
set.seed(90)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")      

#prune the tree
prune.carseats=prune.misclass(tree.carseats,best=7)
plot(prune.carseats)
text(prune.carseats,pretty=0)

#calculate test MSE
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
print(paste("The test MSE is ", (34+16)/200))


#________________
#regression tree
library(MASS)
View(Boston)

#划分training set 和 testing set
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

#calcaulate test MSE 
pred = predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
mean((pred-boston.test)^2)

#using 10-cv 找best tuning parameter：size of tree 和 惩罚系数alpha
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

#prune the tree
prune.boston=prune.tree(tree.boston,best=7)
plot(prune.boston)
text(prune.boston,pretty=0)

#calculate test MSE
yhat=predict(prune.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)


# Bagging 
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

#calculate test MSE
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

#visualize the importance plot
importance(bag.boston)
varImpPlot(bag.boston)

#another argument is that we can change the number of tree
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
bag.boston
mean((yhat.bag-boston.test)^2)


#________________________________
#randomforeset
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

importance(rf.boston)
varImpPlot(rf.boston)


#_____________________
#Boosting for the regression tree
library(gbm)
set.seed(13)

#build the model 
boost.boston=gbm(medv~.,data=Boston[train,],
                  distribution="gaussian",n.trees=5000,
                  interaction.depth=1)
summary(boost.boston)
plot(boost.boston,i="lstat")

#calculate test MSE
yhat.boost=predict(boost.boston,newdata=Boston[-train,],
                   n.trees=5000)
mean((yhat.boost-Boston[-train,]$medv)^2)


#find the best number of tree B
number_of_tree = c( 5000, 10000, 15000, 20000 )
nt = length(number_of_tree)
cv.error = rep(0,4)

for( i in 1:nt ){ # for each number of cuts to test
  n_tree = number_of_tree[i] 
  boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=n_tree,interaction.depth=1)
  yhat.boost=predict(boost.boston,newdata=Boston[-train,],
                     n.trees=n_tree)
  cv.error[i] =mean((yhat.boost-Boston[-train,]$medv)^2)
}
  
cv.error
