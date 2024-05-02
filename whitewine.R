library(factoextra)
library(NbClust)
library(cluster)
library(stats)
library(fpc)
library(clusterCrit)

library(readxl)

#reading the dataset 
whitewine<-read_excel(path="datasets/Whitewine_v6.xlsx")
summary(whitewine)

# Define a function to replace outliers with NA
remove_outliers <- function(x) {
  lowerq <- quantile(x, probs = 0.25)
  upperq <- quantile(x, probs = 0.75)
  iqr <-IQR(x)
  
  x_cleaned<-ifelse(x<lowerq-1.5*iqr|x>upperq +1.5*iqr,NA,x)
  return(x_cleaned)
}

wine_out_rm<-as.data.frame(lapply(whitewine,remove_outliers))


# Count removed outliers per column (assuming numerical data)
removed_per_col <- apply(wine_out_rm, 2, function(x) sum(is.na(x)))
cat("\nNumber of outliers removed per column:\n")
print(removed_per_col)

wine_cleaned<- na.omit(wine_out_rm) 
boxplot(wine_cleaned,outline=FALSE)

#scaling the dataset
wine_subset <- as.data.frame(scale(wine_cleaned[,1:11]))
boxplot(wine_subset,outline=FALSE)  


#determining the clusters
set.seed(123)

#nbclust method
nb<-NbClust(wine_subset,distance = "euclidean",min.nc = 2,max.nc = 10,method = "kmeans")

#elbow method
fviz_nbclust(wine_subset, kmeans, method="wss")

#silhouette method
fviz_nbclust(wine_subset,kmeans,method="silhouette")

#gap statistic method
fviz_nbclust(wine_subset,kmeans,method="gap_stat",B=)


#kmeans clustering
kmeans_wine=kmeans(wine_subset,centers = 2,nstart = 10)
kmeans_wine
fviz_cluster(kmeans_wine,data=wine_subset)

wine_centers<-kmeans_wine$centers
cat("\n Cluster Centers: \n")
print(wine_centers)

# Within-Cluster Sum of Squares and Total Sum of Squares
wss <- kmeans_wine$tot.withinss
cat("\n Within cluster sums of squares : \n")
wss

tss <- kmeans_wine$totss
cat("\n Total sum of squares: \n")
tss

# Between-Cluster Sum of Squares (BSS)
bss <-kmeans_wine$betweenss
cat("\n Between-Cluster Sum of Squares: \n")
bss

# Ratio of BSS/TSS
bss_over_tss <- bss / tss
cat("\n Ratio between BSS/TSS: \n")
bss_over_tss

#silhouette analysis
silhouette_wine<-silhouette(kmeans_wine$cluster,dist(wine_subset)) 
#need to use $cluster because just kmeans give other info
fviz_silhouette(silhouette_wine)



#pca
wine_pca<-prcomp(wine_subset,center = TRUE,scale = TRUE)
summary(wine_pca)

#eigenvalues/eigenvectors
eigenvalues<-(wine_pca$sdev)^2 #standard deviations (square roots of eigenvalues)
cat("\n eigenvalues: \n")
print(eigenvalues)

eignenvectors<-wine_pca$rotation
cat("\n eigenvectors: \n")
print(eignenvectors)

# Calculate cumulative proportion
cumulative_proportion <- cumsum(eigenvalues)/ sum(eigenvalues)
cat("\n Cumulative proportion: \n")
cumulative_proportion

# Plot cumulative score
plot(cumulative_proportion, type = "b", 
     xlab = "Number of Principal Components",
     ylab = "Explained Varience",
     main = "Cumulative Score per Principal Components")
abline(0.85,0,col="red",lty=2)

#transformed dataset
wine_transform=as.data.frame(-wine_pca$x[,1:7])
head(wine_transform)

#determining the clusters for pca

#nbclust method
nb<-NbClust(wine_transform,distance = "euclidean",min.nc = 2,max.nc = 10,method = "kmeans")

#elbow method
fviz_nbclust(wine_transform, kmeans, method="wss")

#Silhouette method
fviz_nbclust(wine_transform,kmeans,method="silhouette")

#gap statistic method
fviz_nbclust(wine_transform,kmeans,method="gap_stat")

#kmeans clustering
kmeans_wine_transform<-kmeans(wine_transform,centers = 2,nstart = 10)
kmeans_wine_transform

wine_centers_pca<-kmeans_wine_transform$centers
cat("\n Cluster Centers: \n")
print(wine_centers_pca)

# Within-Cluster Sum of Squares and Total Sum of Squares
wss_pca <- kmeans_wine_transform$tot.withinss
cat("\n Within cluster sums of squares : \n")
wss_pca

#total sum of squares(TSS)
tss_pca <- kmeans_wine_transform$totss
cat("\n Total sum of squares: \n")
tss_pca

# Between-Cluster Sum of Squares (BSS)
bss_pca <-kmeans_wine_transform$betweenss
cat("\n Between-Cluster Sum of Squares: \n")
print(bss_pca)

# Ratio of BSS/TSS
bss_over_tss_pca <- bss_pca / tss_pca
cat("\n Ratio between BSS/TSS: \n")
bss_over_tss_pca


#silhouette analysis
silhouette_wine_pca<-silhouette(kmeans_wine_transform$cluster,dist(wine_transform)) 
#need to use $cluster because just kmeans give other info
fviz_silhouette(silhouette_wine_pca)


#Calinski-Harabasz Index 
ch<-calinhara(wine_transform,kmeans_wine_transform$cluster)
cat("Calinski-Harabasz Index:", ch, "\n")


fviz_ch <- function(data) {
  
  ch_index_values <- numeric(length = 10)
  for (i in 2:10) {
    kmeans_results <- kmeans(data, centers = i, nstart = 10)
    ch_index_values[i] <- calinhara(data, kmeans_results$cluster)
  }
  
  # Plot the CH Index values vs number of clusters
  plot(1:10, ch_index_values, type = "b",
       xlab = "Number of Clusters (k)",
       ylab = "Calinski-Harabasz Index",
      main = "Caliński - Harabasz Plot" )
  abline(v = 2, col = "red", lty = 2)
}

fviz_ch(wine_transform)







