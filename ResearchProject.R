install.packages("factoextra")
library(factoextra)

library(readxl)
GoodData <- read_excel("C:/Users/vince/Downloads/GoodData.xlsx")
GoodData <- GoodData[-c(170, 152, 41, 136, 188, 315, 290, 186),] #outliers found in lines 32-36

View(GoodData)


#Data Labels
data.labels = GoodData[, c("NPI", "Providers with no missing data", "Specialty", "User Type", "Gender", "Medical school Graduation Year", "Years Since Graduation")]
table(data.labels)

# Putting all useful data into one object
EHR_data <- GoodData[8:13]

# Scaling data (to unweight data for Euclidean distances)
EHR_data_scale <- scale(EHR_data)

# Distance
EHR_data <- dist(EHR_data_scale)

# Calculate how many clusters you need (K) using within sum squares (wss)
fviz_nbclust(EHR_data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

# Kmeans Clustering fr
km.out <- kmeans(EHR_data_scale, centers = 2, nstart = 100)
print(km.out)

centers <- km.out$centers[km.out$cluster, ]
distances <- sqrt(rowSums((EHR_data_scale - centers)^2))
outliers <- order(distances, decreasing=T)[1:8]
print(outliers)
print(EHR_data_scale[outliers,])

# Visualize the clustering algorithm results
km.clusters <- km.out$cluster
fviz_cluster(list(data=EHR_data_scale, cluster = km.clusters))

# readding cluster identification to data frame
GoodData$km.clusters <- km.clusters
View(GoodData)
write.csv(GoodData, file = "AssignedData.csv")




# Test space:
# Removing "outliers"
GoodData <- GoodData[-c(170, 152, 41, 136, 188, 315, 290, 186),]

# Putting all useful data into one object
EHR_data <- GoodData[8:13]

# Scaling data (to unweight data for Euclidean distances)
EHR_data_scale <- scale(EHR_data)
# Distance
EHR_data <- dist(EHR_data_scale)

# Calculate how many clusters you need (K) using within sum squares (wss)
fviz_nbclust(EHR_data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

# Kmeans Clustering fr
km.out <- kmeans(EHR_data_scale, centers = 5, nstart = 100)
print(km.out)

# Visualize the clustering algorithm results
km.clusters <- km.out$cluster
fviz_cluster(list(data=EHR_data_scale, cluster = km.clusters))
