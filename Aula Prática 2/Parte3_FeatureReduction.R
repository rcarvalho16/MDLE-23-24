#install.packages(c("ggcorrplot", "factoextra"))
library("ggcorrplot")
library("factoextra")

convertQualitativeFeatures = function(dataset){
  for(feature in colnames(dataset)){
    classes <- sapply(dataset[feature], class)
    
    # If the column is of type numeric or integer there is no need of conversion
    if(any(c("numeric", "integer") %in% classes)){
      next
    }
    dataset[feature] = as.numeric(factor(dataset[[feature]],
                                         labels = seq(1, length(unique(factor(dataset[[feature]])))),
                                         ordered = TRUE))
  }
  return(dataset)
}

data_lisbon <- read.csv("Lisbon_ 2023-01-01_2023-01-31.csv")
data_pima <- read.csv("pima.csv")

# Convert all the non numerical columns
data_lisbon <- convertQualitativeFeatures(data_lisbon)
data_pima <- convertQualitativeFeatures(data_pima)

# Normalizes the data
normalized_data_lisbon <- scale(data_lisbon)
normalized_data_pima <- scale(data_pima)

# Computes the correlation matrixes
corr_matrix_lisbon <- cor(normalized_data_lisbon)
corr_matrix_pima <- cor(normalized_data_pima)

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
summary(data_lisbon_pca)
summary(data_pima_pca)

# Plot PCA results
fviz_eig(data_lisbon_pca, addlabels = TRUE)
fviz_eig(data_pima_pca, addlabels = TRUE)

# b)

data_lisbon_svd <- svd(corr_matrix_lisbon)
data_pima_svd <- svd(corr_matrix_pima)

# Falta confirmar isto
plot(1:length(data_lisbon_svd$d), data_lisbon_svd$d)
plot(1:length(data_pima_svd$d), data_pima_svd$d)

# c)

# rd = reduced dimension
rd_normalized_data_lisbon <- normalized_data_lisbon[, 1:3]
rd_normalized_data_pima <- normalized_data_pima[, 1:5]

# Computes the correlation matrixes
rd_corr_matrix_lisbon <- cor(rd_normalized_data_lisbon)
rd_corr_matrix_pima <- cor(rd_normalized_data_pima)

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

# Falta confirmar isto
plot(1:length(rd_data_lisbon_svd$d), rd_data_lisbon_svd$d)
plot(1:length(rd_data_pima_svd$d), rd_data_pima_svd$d)
