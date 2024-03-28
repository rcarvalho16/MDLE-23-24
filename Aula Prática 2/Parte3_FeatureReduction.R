#install.packages(c("ggcorrplot", "factoextra"))
library("ggcorrplot")
library("factoextra")

################### AUX FUNCTIONS #############################

getCategoricalFeatures = function(dataset){
  feature_types = sapply(dataset, class)
  names(feature_types)[!(feature_types %in% c("numeric", "integer"))]
}

getClassMeans = function(dataset, classFeature){
  labels_feature = unique(dataset[[classFeature]])
  return(sapply(labels_feature, function(label) round(colMeans(dataset[dataset[[classFeature]] == label,]), digits = 2)))
}

getClassVars = function(dataset, classFeature){
  labels_feature = unique(dataset[[classFeature]])
  return(sapply(labels_feature, function(label) round(sapply(dataset[dataset[[classFeature]] == label,], var), digits = 2)))
}

convertCategoricalFeatures = function(dataset){
  
  categorical_features = getCategoricalFeatures(dataset)
  
  for(feature in categorical_features){
    # Determine different categorical values in feature
    categorical_types_of_feature = unique(dataset[[feature]])
    
    # Calculate relative frequency of each unique value
    relative_freq = sapply(categorical_types_of_feature, function(value) {
      round(sum(dataset[[feature]] == value) / length(dataset[[feature]]), digits = 4)
    })
    
    # Replace categorical feature with equivalent relative frequency
    dataset[[feature]] = sapply(dataset[[feature]], function(value) {
      relative_freq[match(value, categorical_types_of_feature)]
    })
  }
  return(dataset)
}

############################################################

data_lisbon <- read.csv("Lisbon_ 2023-01-01_2023-01-31.csv")
data_pima <- read.csv("pima.csv")

# Conversion of categorical features based on relative frequency
data_lisbon <- convertCategoricalFeatures(data_lisbon)
data_pima <- convertCategoricalFeatures(data_pima)

# Computes the correlation matrixes
corr_matrix_lisbon <- cor(data_lisbon)
corr_matrix_pima <- cor(data_pima)

# Replace NAs with 0s using replace()
corr_matrix_lisbon <- replace(corr_matrix_lisbon, is.na(corr_matrix_lisbon), 0)
corr_matrix_pima <- replace(corr_matrix_pima, is.na(corr_matrix_pima), 0)

# Plot the correlation matrixes
ggcorrplot(corr_matrix_lisbon)
ggcorrplot(corr_matrix_pima)

# a)

# Apply PCA to the correlation matrixes
data_lisbon_pca <- prcomp(corr_matrix_lisbon, scale. = FALSE)
data_pima_pca <- prcomp(corr_matrix_pima, scale. = FALSE)
summary_lisbon_pca <- summary(data_lisbon_pca)
summary_pima_pca <- summary(data_pima_pca)

# The adequate dimensions are the ones with a cumulative proportion of variance of at least 80%
adequate_dimensions_lisbon_pca <- min(which(summary_lisbon_pca$importance[3,] > 0.8))
adequate_dimensions_pima_pca <- min(which(summary_pima_pca$importance[3,] > 0.8))

# Plot PCA results
fviz_eig(data_lisbon_pca, addlabels = TRUE)
fviz_eig(data_pima_pca, addlabels = TRUE)

# b)

data_lisbon_svd <- svd(corr_matrix_lisbon)
data_pima_svd <- svd(corr_matrix_pima)

# 
plot(data_lisbon_svd$d)
plot(data_pima_svd$d)

# Cumulative plot
plot(cumsum(data_lisbon_svd$d^2)/sum(data_lisbon_svd$d^2)*100,ylab="Percent variance",ylim=c(0,100),type="l")
plot(cumsum(data_pima_svd$d^2)/sum(data_pima_svd$d^2)*100,ylab="Percent variance",ylim=c(0,100),type="l")

# The adequate dimensions are the ones with a cumulative proportion of variance of at least 80%
adequate_dimensions_lisbon_svd <- min(which(cumsum(data_lisbon_svd$d^2)/sum(data_lisbon_svd$d^2) > 0.8))
adequate_dimensions_pima_svd <- min(which(cumsum(data_pima_svd$d^2)/sum(data_pima_svd$d^2) > 0.8))

# c)

# rd = reduced dimension
rd_data_lisbon <- data_lisbon[, 1:adequate_dimensions_lisbon_pca]
rd_data_pima <- data_pima[, 1:adequate_dimensions_pima_pca]

# Computes the correlation matrixes
rd_corr_matrix_lisbon <- cor(rd_data_lisbon)
rd_corr_matrix_pima <- cor(rd_data_pima)

# Replace NAs with 0s using replace()
rd_corr_matrix_lisbon <- replace(rd_corr_matrix_lisbon, is.na(rd_corr_matrix_lisbon), 0)
rd_corr_matrix_pima <- replace(rd_corr_matrix_pima, is.na(rd_corr_matrix_pima), 0)

# Plot the correlation matrixes
ggcorrplot(rd_corr_matrix_lisbon)
ggcorrplot(rd_corr_matrix_pima)

# Apply PCA to the correlation matrixes
rd_data_lisbon_pca <- prcomp(rd_corr_matrix_lisbon, scale. = FALSE)
rd_data_pima_pca <- prcomp(rd_corr_matrix_pima, scale. = FALSE)
summary(rd_data_lisbon_pca)
summary(rd_data_pima_pca)

# Plot PCA results
fviz_eig(rd_data_lisbon_pca, addlabels = TRUE)
fviz_eig(rd_data_pima_pca, addlabels = TRUE)

rd_data_lisbon_svd <- svd(corr_matrix_lisbon)
rd_data_pima_svd <- svd(corr_matrix_pima)

#
plot(1:length(rd_data_lisbon_svd$d), rd_data_lisbon_svd$d)
plot(1:length(rd_data_pima_svd$d), rd_data_pima_svd$d)

# Cumulative plot
plot(cumsum(rd_data_lisbon_svd$d^2)/sum(rd_data_lisbon_svd$d^2)*100,ylab="Percent variance",ylim=c(0,100),type="l")
plot(cumsum(rd_data_pima_svd$d^2)/sum(rd_data_pima_svd$d^2)*100,ylab="Percent variance",ylim=c(0,100),type="l")
