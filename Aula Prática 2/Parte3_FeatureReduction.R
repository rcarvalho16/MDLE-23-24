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


# Analysing the correlation between features before performing PCA
# Is good practice. Generally, non-correlated features don't work well with PCA
# However, given the Lab guide, this will be performed anyway.
# Ref: http://www.sthda.com/english/wiki/wiki.php?id_contents=7866

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
data_lisbon_pca <- prcomp(data_lisbon, scale. = FALSE)
data_pima_pca <- prcomp(data_pima, scale. = FALSE)
summary_lisbon_pca <- summary(data_lisbon_pca)
summary_pima_pca <- summary(data_pima_pca)

# Quick summary of PCA results:
summary_lisbon_pca
summary_pima_pca

# The adequate dimensions are the ones with a cumulative proportion of variance of at least 90%
adequate_dimensions_lisbon_pca <- min(which(summary_lisbon_pca$importance[3,] > 0.9))
adequate_dimensions_pima_pca <- min(which(summary_pima_pca$importance[3,] > 0.9))

# Plot PCA results
# fviz_eig() plots the eigen values against the number of dimensions
# You can also calculate the eigenvalues by squaring the standard deviation in pca
# data_lisbon_pca$sdev ^ 2
fviz_eig(data_lisbon_pca, addlabels = TRUE)
fviz_eig(data_pima_pca, addlabels = TRUE)

# b)

# Compute the SVD matrices for each dataset
data_lisbon_svd <- svd(data_lisbon)
data_pima_svd <- svd(data_pima)
singular_values_lisbon <- data_lisbon_svd$d
singular_values_pima <- data_pima_svd$d

par(mfrow = c(1, 2))
# Plot singular values sorted in decreasing order for both datasets
plot(singular_values_lisbon, type = "b", xlab = "Component", ylab = "Singular Value",
     main = "Singular Values Sorted in Decreasing Order (Data Lisbon)")
plot(singular_values_pima, type = "b", xlab = "Component", ylab = "Singular Value",
     main = "Singular Values Sorted in Decreasing Order (Data Pima)")



# The adequate dimensions are the ones with a cumulative proportion of variance of at least 95%
# O SVD em lisboa não funciona para menos de 99% de relevância porque só fica com 1 coluna...
adequate_dimensions_lisbon_svd <- min(which(cumsum(data_lisbon_svd$d^2)/sum(data_lisbon_svd$d^2) > 0.99))
adequate_dimensions_pima_svd <- min(which(cumsum(data_pima_svd$d^2)/sum(data_pima_svd$d^2) > 0.95))

# c)

# FOR PCA NÃO FAÇO IDEIA

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

# For SVD reduction the dimensionality reduction is done by neglecting the matrix V as 
# it is used only for mapping the data to the reduced dimension space
# so, we only need the 'U' and 'D' matrices, and the adequate number of dimensions

reduced_lisbon_svd <- data_lisbon_svd$u[, 1:adequate_dimensions_lisbon_svd] %*% diag(data_lisbon_svd$d[1:adequate_dimensions_lisbon_svd]) 
reduced_pima_svd <- data_pima_svd$u[, 1:adequate_dimensions_pima_svd] %*% diag(data_pima_svd$d[1:adequate_dimensions_pima_svd]) 

# Computes the correlation matrixes
rd_corr_matrix_lisbon_svd <- cor(reduced_lisbon_svd)
rd_corr_matrix_pima_svd <- cor(reduced_pima_svd)

# Replace NAs with 0s using replace()
rd_corr_matrix_lisbon_svd <- replace(rd_corr_matrix_lisbon_svd, is.na(rd_corr_matrix_lisbon_svd), 0)
rd_corr_matrix_pima_svd <- replace(rd_corr_matrix_pima_svd, is.na(rd_corr_matrix_pima_svd), 0)

# Plot the correlation matrixes
ggcorrplot(rd_corr_matrix_lisbon_svd)
ggcorrplot(rd_corr_matrix_pima_svd)


