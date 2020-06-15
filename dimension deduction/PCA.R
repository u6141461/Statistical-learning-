library("ISLR")
View(College)

#划分前600个样本为training test，其余177个为testing set
train=(0:600)
training_set = College[train,]
test=(-train)
testing_set = College[(-train),]

#biplot
training_set = data.matrix(training_set[,-2])
pr.out=prcomp(training_set, scale=TRUE)
biplot(pr.out)

#Proportion of Variance Explained
pr.var=pr.out$sdev^2
plot(pr.var, ylab="Variance", xlab="Principal Component",type='b',xlim = c(0,10))

#Cumulative Proportion of Variance Explained
pve = cumsum(pr.var)/cumsum(pr.var)[length(pr.var)]

plot(pve, ylab="Cumulative Proportion of Variance Explained", xlab="Principal Component", ylim=c(0,1),type='b')

