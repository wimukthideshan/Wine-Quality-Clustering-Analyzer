#importing a libraries
library(readxl)
library(fpc)
library(NbClust)
library(ggplot2)
library(factoextra)
library(cluster)

#PART A
#reading the xlsx file and saving it to a data frame
df <- read_excel(file.path("F:", "IIT", "2nd Year", "Sem 2", "ML", "CW", "ML CW", "Whitewine_v6.xlsx"))
#removing sample number and class columns
df<-df[,1:11]
#display the data frame
df
#boxplot to visualize outliers
boxplot(df, outline = TRUE)

q1 <- apply(df, 2, quantile, probs = 0.25, na.rm = TRUE)
q3 <- apply(df, 2, quantile, probs = 0.75, na.rm = TRUE)
#calculating the inter quartile range
iqr <- q3 - q1
#finding outliers that are not in the range
outliers <- apply(df, 1, function(x) any(x < q1 - 1.5*iqr | x > q3 + 1.5*iqr))

#removing the outliers
df_no_outliers <- df[!outliers, ]
#boxplot to visualize outliers
boxplot(df_no_outliers, outline = TRUE)
#normalizing with z score
df_no_outliers <- as.data.frame(scale(df_no_outliers))
#boxplot to visualize normalization
boxplot(df_no_outliers, outline = TRUE)
#############################################################
#PART B

str(df_no_outliers)
head(df_no_outliers)
#Nbclust  
set.seed(26)
#euclidean
clusterNo=NbClust(df_no_outliers,distance="euclidean", 
                  min.nc=2,max.nc=10,method="kmeans",index="all")
#manhattan
clusterNo=NbClust(df_no_outliers,distance="manhattan", 
                  min.nc=2,max.nc=10,method="kmeans",index="all")

#maximum
clusterNo=NbClust(df_no_outliers,distance="maximum", 
                  min.nc=2,max.nc=10,method="kmeans",index="all")
#Elbow

fviz_nbclust(df_no_outliers, kmeans, method = 'wss')
#gap
fviz_nbclust(df_no_outliers, kmeans, method = 'gap_stat')
#silhouette
fviz_nbclust(df_no_outliers, kmeans, method = 'silhouette')
#PART c
#for k=2
k = 2
kmeans_df = kmeans(df_no_outliers, centers = k, nstart = 10)
kmeans_df
#visualizing
fviz_cluster(kmeans_df, data = df_no_outliers)
wss = kmeans_df$withinss
bss = kmeans_df$betweenss
tss = kmeans_df$totss
cat("The WSS value is:", wss,"\n")
cat("The BSS value is:", bss,"\n")
cat("Ratio:", bss/tss,"\n" )
cat("Ratio:", bss/wss,"\n" )

#PART d

sil <- silhouette(kmeans_df$cluster, dist(df_no_outliers))
fviz_silhouette(sil)
#Task 2
#PART e
#calculate the covariance matrix
df.cov<-cov(df_no_outliers)
#calculate eigen values and eigen vectors
df.eigen <- eigen(df.cov)
str(df.eigen)
#calculate the PVE
varianceDF <- df.eigen$values / sum(df.eigen$values) * 100
varianceDF
#calculate the cumulative scores
cumulativeDF <- cumsum(varianceDF)
cumulativeDF
#getting the PCs that are > 85
leastPCA <- which(cumulativeDF > 85)
leastPCA
#assigning eigen vectors to eig_matrix

eig_matrix<-df.eigen$vectors
#as it is by default in negative direction turning it to positive direction by adding a - sign
eig_matrix<- -eig_matrix

# Check dimensions of eigenvector matrix
dim(df.eigen$vectors)

row.names(eig_matrix) <- c("Fixed Acidity","Volatile Acidity", "Citric Acid", "Residual Sugar", "Chlorides",
                           "Free SO2", "Total SO2", "Density", "pH", "Sulphates", "Alcohol")
colnames(eig_matrix) <- c("PC1", "PC2","PC3", "PC4","PC5", "PC6","PC7", "PC8","PC9", 
                          "PC10","PC11")
eig_matrix
#calculating principal component scores
PC1 <- as.matrix(df_no_outliers) %*% eig_matrix[, 1]
PC2 <- as.matrix(df_no_outliers) %*% eig_matrix[, 2]
PC3 <- as.matrix(df_no_outliers) %*% eig_matrix[, 3]
PC4 <- as.matrix(df_no_outliers) %*% eig_matrix[, 4]
PC5 <- as.matrix(df_no_outliers) %*% eig_matrix[, 5]
PC6 <- as.matrix(df_no_outliers) %*% eig_matrix[, 6]
PC7 <- as.matrix(df_no_outliers) %*% eig_matrix[, 7]
PC8 <- as.matrix(df_no_outliers) %*% eig_matrix[, 8]
PC9 <- as.matrix(df_no_outliers) %*% eig_matrix[, 9]
PC10 <- as.matrix(df_no_outliers) %*% eig_matrix[, 10]
PC11 <- as.matrix(df_no_outliers) %*% eig_matrix[, 11]

PC <- data.frame( PC1, PC2,PC3,PC4,PC5, PC6,PC7,PC8,PC9, PC10,PC11)
#creating the transformed data frames
transformed_df <- PC[, leastPCA]
head(transformed_df)
#PART f
#Nbclust
set.seed(26)
#euclidean
clusterNo=NbClust(transformed_df,distance="euclidean", 
                  min.nc=2,max.nc=20,method="kmeans",index="all")
#manhattan
clusterNo=NbClust(transformed_df,distance="manhattan", 
                  min.nc=2,max.nc=10,method="kmeans",index="all")
#maximum
clusterNo=NbClust(transformed_df,distance="maximum", 
                  min.nc=2,max.nc=10,method="kmeans",index="all")
#Elbow

fviz_nbclust(transformed_df, kmeans, method = 'wss')
#gap
fviz_nbclust(transformed_df, kmeans, method = 'gap_stat')
#silhouette
fviz_nbclust(transformed_df, kmeans, method = 'silhouette')
#PART g
#k=2
k = 2
kmeans_tdf = kmeans(transformed_df, centers = k, nstart = 10)
kmeans_tdf
#visualizing
fviz_cluster(kmeans_tdf, data = transformed_df)
wss = kmeans_tdf$tot.withinss
bss = kmeans_tdf$betweenss
tss = kmeans_tdf$totss
cat("The WithinCluster Sum Square value is:", wss,"\n")
cat("The Between Sum Square value is:", bss,"\n")
cat("Ratio bss to tss:", bss/tss,"\n" )
cat("Ratio bss to wss:", bss/wss,"\n" )
center = kmeans_tdf$centers
center

#k=3
k = 3
kmeans_tdf = kmeans(transformed_df, centers = k, nstart = 10)
kmeans_tdf
#visualizing
fviz_cluster(kmeans_tdf, data = transformed_df)
wss = kmeans_tdf$tot.withinss
bss = kmeans_tdf$betweenss
tss = kmeans_tdf$totss
cat("The WithinCluster Sum Square value is:", wss,"\n")
cat("The Between Sum Square value is:", bss,"\n")
cat("Ratio bss to tss:", bss/tss,"\n" )
cat("Ratio bss to wss:", bss/wss,"\n" )
center = kmeans_tdf$centers
center

#PART h
#library(clusterSS)
sil <- silhouette(kmeans_tdf$cluster, dist(transformed_df))
fviz_silhouette(sil)
#PART i



fit_ch <- kmeansruns(transformed_df, krange = 1:10, criterion = "ch")
plot(1:10, fit_ch$crit, type = "o", col = "red", pch = 0, xlab = "Number of clusters", ylab = "CalinskiHarabasz criterion")
cat("The best k is ",fit_ch$bestk,"\n")