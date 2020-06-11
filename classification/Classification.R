library(ISLR)
View(iris)

#划分 training set 和testing set
train = sample(1:nrow(iris),nrow(iris)*3/4)
train_set = iris[train,]
test_set = iris[-(train),]

#——————————————————————————————————————————————————————————————————————
#(1) LDA
#build LDA model
library(MASS)
lda.fit<-lda(Species~.,data = train_set)
plot(lda.fit)

#calculate test MSE
lda.pred<-predict(lda.fit,test_set)
ConfusionMatrix <- table(lda.pred$class,test_set$Species)
ConfusionMatrix
mean(test_set$Species!=lda.pred$class)


#(2) QDA
#build LDA model
qda.fit<-qda(Species~.,data = train_set)

#calculate test MSE
qda.pred<-predict(qda.fit,test_set)
ConfusionMatrix <- table(qda.pred$class,test_set$Species)
ConfusionMatrix
mean(test_set$Species!=qda.pred$class)


#(3) Logistics
require(nnet)
multi.fitting<- multinom(Species~.,data=train_set)
#glm.fitting<- glm(Species~.,data=train_set,family = multinomial(link = logit))
logistics.pred = predict(multi.fitting, newdata = test_set, "class")

#calculate test MSE
ConfusionMatrix <- table(logistics.pred,test_set$Species)
ConfusionMatrix
mean(test_set$Species!=logistics.pred)

#(4) KNN
library(class)
knn(train_set[,-5],test_set[,-5],train_set[,5],k=1)

mis_class_error_rate={}
i = 0
while(i<=20){
  i = i+1
  knn.prediction = knn(train_set[,-5],test_set[,-5],train_set[,5],k=i)
  mis_class_error_rate = c(mis_class_error_rate,mean(test_set$Species!=knn.prediction))
}

plot(mis_class_error_rate,ylab = "miss classification error rate",xlab = "K_th",pch=16,col="red",type = "o")
which(mis_class_error_rate==min(mis_class_error_rate))
min(mis_class_error_rate)
