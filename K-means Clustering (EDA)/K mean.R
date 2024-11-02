
claims_data <- read.csv("UNSW_claims_data.csv")

data_scaled <- c(claims_data$tenure,claims_data$claim_paid,claims_data$total_claim_amount)

wss <- sapply(1:10, function(k) {
  kmeans(data_scaled, centers = k, nstart = 20)$tot.withinss
})
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")

### k = 3

set.seed(123)  
k <- 3  
kmeans_result <- kmeans(data_scaled, centers = k, nstart = 20)

print(kmeans_result$centers)  # Cluster centers
print(kmeans_result$cluster)  # Cluster assignment of each data point

library(ggplot2)

head(data_clustered)
data_clustered <- data.frame(data_scaled, Cluster = as.factor(kmeans_result$cluster))
data_clustered$Cluster <- as.factor(kmeans_result$cluster) 
ggplot(data_clustered, aes(x = data_scaled, y = Cluster, color = Cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "Feature 1", y = "Feature 2")



### k = 4

set.seed(123)  
k <- 4
kmeans_result <- kmeans(data_scaled, centers = k, nstart = 20)

print(kmeans_result)
print(kmeans_result$centers)  # Cluster centers
print(kmeans_result$cluster)  # Cluster assignment of each data point

head(data_clustered)
data_clustered_4 <- data.frame(data_scaled, Cluster = as.factor(kmeans_result$cluster))
data_clustered_4$Cluster <- as.factor(kmeans_result$cluster) 
ggplot(data_clustered_4, aes(x = data_scaled, y = Cluster, color = Cluster)) +
  geom_point(alpha = 1.5)



k <- 3

# Create a data frame with separate columns
data_scaled <- scale(claims_data[, c("tenure", "claim_paid", "total_claim_amount")])
head(claims_data)
head(data_scaled)
data_scaled <- as.data.frame(data_scaled)

# Perform k-means clustering with k = 4
kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 20)

# Add clusters to the data frame
data_clustered <- data.frame(data_scaled, Cluster = as.factor(kmeans_result$cluster))
summary(data)
# Plot with ggplot2
library(ggplot2)
ggplot(data_clustered, aes(x = tenure, y = claim_paid, color = Cluster)) +  # Replace V1 and V2 with feature names if available
  geom_point() +
  labs(title = "K-means Clustering", x = "Tenure (Scaled)", y = "Claim Paid")
