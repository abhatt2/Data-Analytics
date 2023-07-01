#Aastha Bhatt

#load libraries
library(cluster)
library(fpc)
library(scales)
library(ggplot2)

#Read customer data csv file
heart <- read.csv(file="heart.csv", stringsAsFactors = TRUE)
View( heart)
summary(heart)

# Producing a dot plot with sorted categories using WVPlots ClevelandDotPlot()
# if needed install WVPlots package: install.packages("WVPlots")
library(WVPlots)                                              	 
ClevelandDotPlot(heart, "chol",               	 
                 sort = 1, color="red", title="The Number of Patients per Cholestrol Level ") +                   	
  coord_flip()+
  theme(axis.text.y = element_text(size=5)) 


#Create a scatterplot of age and presence of heart disease 
ggplot(heart, aes(x=age, y=target)) +                                  	 
  geom_point() +
  ggtitle("Presence of heart disease compared with age") +
  xlab("Age") +
  ylab("Presence of Heart Disease")


heart <- heart[,c(-3,-7,-9,-10,-11,-12,-13,-14)]
summary(heart)


#Use rescale function to normalize all attributes on a scale betweek 0 and 1
heart_norm <- heart
heart_norm$age <- rescale(heart$age, to = c(0,1))
heart$sex <- rescale(heart$sex , to = c(0,1))
heart$trestbps <- rescale(heart$trestbps, to = c(0,1))
heart$chol <- rescale(heart$chol, to = c(0,1))
heart$fbs <- rescale(heart$fbs, to = c(0,1))
heart$thalach <- rescale(heart$thalach, to = c(0,1))


#run k-means with k = 5
km5 <- kmeans(heart_norm, centers = 5) 
km5
####################################################
#run k-means with k = 2
km2 <- kmeans(heart_norm, centers = 2) 
km2
pairs(heart_norm,col = km2$cluster)

####################################################

#run k-means with k = 3
km3 <- kmeans(heart_norm, centers = 3) 
km3

###############################################################################
#evaluating clusters
###############################################################################
#create distance matrix for cluster.stats
distm <- dist(heart_norm)  

###############################################################################
#evaluate k-means results
###############################################################################
#calculate cluster statistics for km2
cstatskm2 = cluster.stats(distm,km2$cluster)
#calculate cluster statistics for km3
cstatskm3 = cluster.stats(distm,km3$cluster) 
#evaluate between and within cluster distances
cstatskm2$average.between
cstatskm2$average.within
cstatskm3$average.between
cstatskm3$average.within

cstatskm5 = cluster.stats(distm,km5$cluster)
cstatskm5$average.between
cstatskm5$average.within

# Silhouette plot
sil2 <- silhouette(km2$cluster,distm)
avg_sil2 <- mean(sil2[,3])
avg_sil2
plot(sil2)

sil3 <- silhouette(km3$cluster,distm)
avg_sil3 <- mean(sil3[,3])
avg_sil3
plot(sil3)

sil5 <- silhouette(km5$cluster,distm)
avg_sil5 <- mean(sil3[,3])
avg_sil5
plot(sil5)


###############################################################################
#Find number of clusters with elbow method kmeans sse
###############################################################################
sse <- 0
count <- 1 #set counter for loop
#Elbow method using k-means and SSE
for (k in 2:10){
  km <- kmeans(heart_norm, centers=k)
  sse[count] <- km$tot.withinss
  count <- count+1
}
plot(c(2:10),sse)
lines(c(2:10),sse)

###############################################################################
#Find number of clusters with elbow method k-means silhouette coefficient
###############################################################################
avg_sil <- 0
count <- 1 #set counter for loop
#Elbow method using k-means and silhouette coefficient
for (k in 2:10){
  km <- kmeans(heart_norm, centers=k)
  sil <- silhouette(km$cluster,distm)
  #print(mean(sil[,3]))
  avg_sil[count] <- mean(sil[, 3])
  count <- count+1
}
plot(c(2:10),avg_sil)
lines(c(2:10),avg_sil)



#run k-means with k = 6
km6 <- kmeans(heart_norm, centers = 6) 
km6

km7 <-kmeans(heart_norm, centers = 7)
km7

#calculate cluster statistics for km2
cstatskm7 = cluster.stats(distm,km7$cluster)

#evaluate between and within cluster distances
cstatskm7$average.between
cstatskm7$average.within


sil7 <- silhouette(km7$cluster,distm)
avg_sil7 <- mean(sil2[,3])
avg_sil7
plot(sil7)




###############################################################################
#evaluate k-means results
###############################################################################
#calculate cluster statistics for km2
cstatskm6 = cluster.stats(distm,km6$cluster)

#evaluate between and within cluster distances
cstatskm6$average.between
cstatskm6$average.within



# Silhouette plot
sil6 <- silhouette(km6$cluster,distm)
avg_sil6 <- mean(sil2[,3])
avg_sil6
plot(sil6)


#run k-means with k = 6
km8 <- kmeans(heart_norm, centers = 8) 
km8

###############################################################################
#evaluate k-means results
###############################################################################
#calculate cluster statistics for km2
cstatskm8 = cluster.stats(distm,km8$cluster)

#evaluate between and within cluster distances
cstatskm8$average.between
cstatskm8$average.within



# Silhouette plot
sil8 <- silhouette(km8$cluster,distm)
avg_sil8 <- mean(sil8[,3])
avg_sil8
plot(sil8)

#run k-means with k = 6
km4 <- kmeans(heart_norm, centers = 4) 
km4

###############################################################################
#evaluate k-means results
###############################################################################
#calculate cluster statistics for km2
cstatskm4 = cluster.stats(distm,km4$cluster)

#evaluate between and within cluster distances
cstatskm4$average.between
cstatskm4$average.within



# Silhouette plot
sil4 <- silhouette(km4$cluster,distm)
avg_sil4 <- mean(sil4[,3])
avg_sil4
plot(sil4)



pairs(heart_norm,col = km7$cluster)
km7$cluster

heart$cluster <- km7$cluster
heart$cluster <- as.factor(km7$cluster)

#Exploring distributions by category with a box plot
ggplot(heart, aes(x=cluster, y=age, fill=cluster)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") +
  labs(fill="cluster") +
  xlab("Cluster") +
  ylab("age")

#Exploring distributions by category with a box plot
ggplot(heart, aes(x=cluster, y=sex, fill=cluster)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") +
  labs(fill="cluster") +
  xlab("Cluster") +
  ylab("sex")


#Exploring distributions by category with a box plot
ggplot(heart, aes(x=cluster, y=trestbps, fill=cluster)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") +
  labs(fill="cluster") +
  xlab("Cluster") +
  ylab("trestbps")

#Exploring distributions by category with a box plot
ggplot(heart, aes(x=cluster, y=chol, fill=cluster)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") +
  labs(fill="cluster") +
  xlab("Cluster") +
  ylab("chol")



#Exploring distributions by category with a box plot
ggplot(heart, aes(x=cluster, y=thalach, fill=cluster)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") +
  labs(fill="cluster") +
  xlab("Cluster") +
  ylab("thalach")




rm(list = ls())
