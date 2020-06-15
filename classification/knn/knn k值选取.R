library(ISLR)
View(iris)

#划分 training set 和testing set
train = sample(1:nrow(iris),nrow(iris)*3/4)
train_set = iris[train,]
test_set = iris[-(train),]

#(4) KNN
#validation test
set.seed(3)
validation = sample(1:nrow(train_set),nrow(train_set)*1/4)
validation_set =train_set[validation,]
trainvalidation_set = train_set[-(validation),]

library(class)
mis_class_error_rate={}
i = 0
while(i<=20){
  i = i+1
  knn.prediction = knn(trainvalidation_set[,-5],validation_set[,-5],trainvalidation_set[,5],k=i)
  mis_class_error_rate = c(mis_class_error_rate,mean(validation_set$Species!=knn.prediction))
}

plot(mis_class_error_rate,ylab = "miss classification error rate",xlab = "K_th",pch=16,col="red",type = "o")
which(mis_class_error_rate==min(mis_class_error_rate))
min(mis_class_error_rate)

#calculate test MSE
test.pred = knn(train_set[,-5],test_set[,-5],train_set[,5],k=14)
mean(test.pred!=test_set[,5])

#n-fold cv test
set.seed(3)
k=5
folds=sample(1:k,nrow(train_set),replace=TRUE)
out.cv = matrix(0, nrow = k, ncol = 10)
colnames(out.cv)=1:10

for (j in 1:k) {
  for (i in 1:10) {
    pred = knn(train_set[folds!=j,-5],train_set[folds==j,-5],train_set[folds!=j,5],k = i)
    missclass = mean((train_set[folds==j,5]!=pred)^2)
    out.cv[j, i] = missclass
  }
}
cv.missclass = apply(out.cv, 2, mean)

plot(cv.missclass,ylab = "5-fold miss classification error rate",xlab = "K_th",pch=16,col="red",type = "o")
which(cv.missclass==min(cv.missclass))

#calculate test missclassification
test.pred = knn(train_set[,-5],test_set[,-5],train_set[,5],k=9)
mean(test.pred!=test_set[,5])
