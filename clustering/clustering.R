library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

#scale the data set
sd.data=scale(nci.data)

#_______________________________________________
#hier. clustering
par(mfrow=c(1,3))
#Euclidean disdance
data.dist=dist(sd.data,method = "euclidean")

#四种算类于类之间距离的方法
par(mfrow=c(1,1))
#default是complete 是找maximize
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
#single 是找minimize
plot(hclust(data.dist, method="single"), labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="centroid"), labels=nci.labs,  main="centroid Linkage", xlab="", sub="",ylab="")

#挑一种方法去做聚类并pruning和visualize
hc.out=hclust(dist(sd.data),method ="complete")
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

#________________________________________________
#kmeans
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
km.clusters

#compare the kmeans and clustering
table(km.clusters,hc.clusters)


#________________________________
#也可以先PCA 再降维
pr.out=prcomp(nci.data, scale=TRUE)

xhc.out=hclust(dist(pr.out$x[,1:5]))
plot(xhc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
abline(h=100, col="red")
table(cutree(xhc.out,4), nci.labs)


#降到俩个维度聚类
xhc.out=hclust(dist(pr.out$x[,1:2]))
plot(xhc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
abline(h=80, col="red")
xhc.clusters = cutree(xhc.out,3)
xhc.clusters

plot(pr.out$x[,1:2], col=(xhc.clusters+1), 
     main="reduce dimension to 2 hier. Clustering Results with K=3", 
     xlab="", ylab="", pch=20, cex=2)
