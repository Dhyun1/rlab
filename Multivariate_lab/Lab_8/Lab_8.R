## data generate
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

## K-Means
plot(x)
km.out.2=kmeans(x,2,nstart=20)
plot(x, col=(km.out.2$cluster+1), main="K-Means Clustering Results with K=2",pch=16)
km.out.2

km.out.3=kmeans(x,3,nstart=20)
km.out.3
plot(x, col=(km.out.3$cluster+1), main="K-Means Clustering Results with K=3",pch=16)

km.out_ns1=kmeans(x,3,nstart=1)
km.out_ns1$tot.withinss

km.out_ns20=kmeans(x,3,nstart=20)
km.out_ns20$tot.withinss


## Hierarchical
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")

x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance",
     