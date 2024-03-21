#install.packages(c("ggcorrplot", "factoextra"))
library("ggcorrplot")
library("factoextra")

data_lisbon <- read.csv("Lisbon_ 2023-01-01_2023-01-31.csv")
data_pima <- read.csv("pima.csv")

# Non numerical columns
numerical_columns_lisbon <- unlist(lapply(data_lisbon, is.numeric), use.names = FALSE)
numerical_columns_pima <- unlist(lapply(data_pima, is.numeric), use.names = FALSE)

# Remove all the non numerical columns
data_lisbon <- data_lisbon[, numerical_columns_lisbon]
data_pima <- data_pima[, numerical_columns_pima]

# Normalizes the data to be between 0 and 1
#normalized_data <- (data - min(data)) / (max(data) - min(data))
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

# Apply PCA to the correlation matrixes
data_lisbon_pca <- prcomp(corr_matrix_lisbon, scale. = FALSE)
data_pima_pca <- prcomp(corr_matrix_pima, scale. = FALSE)

# Plot PCA results
summary(data_lisbon_pca)
fviz_eig(data_lisbon_pca, addlabels = TRUE)

summary(data_pima_pca)
fviz_eig(data_pima_pca, addlabels = TRUE)

#matrix_pca_lisbon <- matrix(unlist(data_lisbon_pca[5]), ncol = 18)
#colnames(matrix_pca_lisbon) <- paste("PC",1:18, sep="")
#rownames(matrix_pca_lisbon) <- colnames(data_lisbon)
#View(matrix_pca_lisbon)