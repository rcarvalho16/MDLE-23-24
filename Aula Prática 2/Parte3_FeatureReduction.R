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

########################################################
# a) PCA Decomposition
########################################################

# Apply PCA to the correlation matrixes
data_lisbon_pca <- prcomp(data_lisbon, scale. = FALSE)
data_pima_pca <- prcomp(data_pima, scale. = FALSE)
summary_lisbon_pca <- summary(data_lisbon_pca)
summary_pima_pca <- summary(data_pima_pca)

# Quick summary of PCA results:
summary_lisbon_pca
summary_pima_pca

# Plot PCA results
# fviz_eig() plots the eigen values against the number of dimensions
# You can also calculate the eigenvalues by squaring the standard deviation in pca
fviz_eig(data_lisbon_pca, addlabels = TRUE)
fviz_eig(data_pima_pca, addlabels = TRUE)

# The eigenvalues display the variance of the original data preserved
eig_values_lisbon = get_eig(data_lisbon_pca)
eig_values_pima = get_eig(data_pima_pca)



########################################################
# b) SVD Decomposition 
########################################################

# Compute the SVD matrices for each dataset
# SVD represents X dataset as a product of three matrices
# X = UDV'
# Note, svd() function produces the left and right singular vectors represented as 'u' and 'v'
# And 'd' is a vector of singular values, corresponding to the diagonal of the matrix 'D'
# Ref: https://bookdown.org/rdpeng/exdata/dimension-reduction.html

data_lisbon_svd <- svd(data_lisbon)
data_pima_svd <- svd(data_pima)
singular_values_lisbon <- data_lisbon_svd$d
singular_values_pima <- data_pima_svd$d


# !!! Duvida, com 90% fico com apenas 1 dimensão adequada não consigo calcular a redução !!!

# The singular values, when squared, are proportional to the amount of variance
# Given by a singular vector.
# Given our 90% threshold, we can compute the adequate dimensions used to compute
# the SVD matrix used in the reduction process
svd_variance_lisbon <- data_lisbon_svd$d^2
svd_variance_pima <- data_pima_svd$d^2
adequate_dimensions_lisbon_svd <- min(which(cumsum(svd_variance_lisbon)/sum(svd_variance_lisbon) > 0.99))
adequate_dimensions_pima_svd <- min(which(cumsum(svd_variance_pima)/sum(svd_variance_pima) > 0.95))

# If we look at the plot of the variances, we can further comprehend how the data can be seen by just
# a few singular vectors
par(mfrow = c(1,2))

# Plot for Lisbon SVD
plot(svd_variance_lisbon/sum(svd_variance_lisbon),
     type = "b",
     xlab = "Singular Vector", ylab = "Variance", 
     main = "Contribution of each vector to the variance of the original data [Lisbon]", 
     pch = 19,
     col = "blue",
)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width

# Plot for pima SVD
plot(svd_variance_pima/sum(svd_variance_pima),
     type = "b",
     xlab = "Singular Vector", ylab = "Variance", 
     main = "Contribution of each vector to the variance of the original data [pima]", 
     pch = 19,
     col = "blue",
)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width


#################################
# c) Dimensionality Reduction
#################################

###################
# PCA Reduction
###################
# Preserve only principal components whose eigen values compute 90% of the variance
adequate_dimensions_lisbon_pca <- min(which(eig_values_lisbon$cumulative.variance.percent > 90))
adequate_dimensions_pima_pca <- min(which(eig_values_pima$cumulative.variance.percent > 90))

# Keep only the PCs that compute minimum adequate dimensions
pcs_lisbon <- data_lisbon_pca$rotation[, 1:adequate_dimensions_lisbon_pca]
pcs_pima <- data_pima_pca$rotation[, 1:adequate_dimensions_pima_pca]

# Compute the newly reduced dataset by applying the product of the original dataset
# With the pca components
rd_pca_lisbon <- as.matrix(data_lisbon) %*% as.matrix(pcs_lisbon)
rd_pca_pima <- as.matrix(data_pima) %*% as.matrix(pcs_pima)

# Quick summary of reduced dataset
summary(rd_pca_lisbon)
summary(rd_pca_pima)

###################
# SVD Reduction
###################

# We can compute the approximation of the original values given our adequate dimensions and the svd
# decomposition now. Remembering X = UDV', with 'U' and 'V' being respectively the left and right singular vector matrices
reduced_lisbon_svd <- data_lisbon_svd$u[, 1:adequate_dimensions_lisbon_svd] %*% diag(data_lisbon_svd$d[1:adequate_dimensions_lisbon_svd]) 
reduced_pima_svd <- data_pima_svd$u[, 1:adequate_dimensions_pima_svd] %*% diag(data_pima_svd$d[1:adequate_dimensions_pima_svd]) %*% t(data_pima_svd$v[,1:adequate_dimensions_pima_svd])

# Computes the correlation matrixes
rd_corr_matrix_lisbon_svd <- cor(reduced_lisbon_svd)
rd_corr_matrix_pima_svd <- cor(reduced_pima_svd)

# Replace NAs with 0s using replace()
rd_corr_matrix_lisbon_svd <- replace(rd_corr_matrix_lisbon_svd, is.na(rd_corr_matrix_lisbon_svd), 0)
rd_corr_matrix_pima_svd <- replace(rd_corr_matrix_pima_svd, is.na(rd_corr_matrix_pima_svd), 0)

# Plot the correlation matrixes
ggcorrplot(rd_corr_matrix_lisbon_svd)
ggcorrplot(rd_corr_matrix_pima_svd)

