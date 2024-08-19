#Loading the data

data<-iris
head(data)
attach(data)
na.omit(data)

#Installing the packages

install.packages("cluster")
library(cluster)
install.packages("ggplot2")
library(ggplot2)

#Preprocess the dataset

Iris_data<-data[,-5]
head(Iris_data)

#Finding Optimum number of cluster using elbow method
#First finding within cluster sum of square

?kmeans

wss<-function(x)
{
  kmeans(Iris_data,x,nstart=5)$tot.withinss
}
xvalue<-1:5
wssvalue<-sapply(xvalue,wss)
wssvalue

#Now plotting the within cluster sum of squares against the number of cluster.

plot(xvalue,wssvalue,type = "b",pch=19,col="green",xlab = "Number of Cluster",ylab = "Total within cluster sum of squares values")

#So we can see that there is an elbow wit respect to the cluster number 3 so we can conclude by method that there is 3 optimum number of clusters.

#We now applying k means clustering

set.seed(100)
kmeans_Cluster<-kmeans(Iris_data,centers = 3,nstart = 5)
kmeans_Cluster

data$Cluster <- as.factor(kmeans_Cluster$cluster)

#Visualize the clusters with 2d plot with Principal component analysis.

PCA<-prcomp(Iris_data,scale. = TRUE)
PCA

PCA_DF<-data.frame(PCA$x[,1:2],cluster=data$Cluster)

ggplot(PCA_DF, aes(x = PC1, y = PC2, color = data$Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering on Iris Data (2D PCA)", x = "PC1", y = "PC2") +
  theme_minimal()
