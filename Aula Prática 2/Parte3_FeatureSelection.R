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



convertQualitativeFeatures = function(dataset){
  
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
  

###############################################################


data_lisbon <- read.csv("Lisbon_ 2023-01-01_2023-01-31.csv")
data_pima <- read.csv("pima.csv")

# Preparing datasets
data_pima = convertQualitativeFeatures(data_pima)
data_lisbon = convertQualitativeFeatures(data_lisbon)


# Alinea (a)

# Step 1: Calculate the variance of each feature
variances_lisbon <- sapply(data_lisbon, var)
variances_pima <- sapply(data_pima, var)

# Sort variances
variances_lisbon = sort(variances_lisbon, decreasing = TRUE)
variances_pima = sort(variances_pima, decreasing = TRUE)

# Step 2: Compute the mean and median of each feature
means_lisbon <- sapply(data_lisbon, mean)
means_pima <- sapply(data_pima, mean)


medians_lisbon <- sapply(data_lisbon, median)
medians_pima <- sapply(data_pima, median)


# Step 3: Compute the absolute difference between the mean and median
mean_median_lisbon <- sort(abs(means_lisbon - medians_lisbon), decreasing = TRUE)
mean_median_pima <- sort(abs(means_pima - medians_pima), decreasing = TRUE)


# Step 4: Plot it
x_axis_lisbon <- as.numeric(seq_along(data_lisbon))
x_axis_pima <- as.numeric(seq_along(data_pima))


par(mfrow = c(2,2))


plot(x = x_axis_lisbon, y = variances_lisbon,
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevância das features com base na variância [Lisbon]", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(x_axis_lisbon)),
     ylim = c(0, max(variances_lisbon) + 0.1 * max(variances_lisbon))
)
grid(nx = NULL, ny = NULL,
           lty = 2,      # Grid line type
           col = "gray", # Grid line color
           lwd = 2)      # Grid line width
for (i in 1:length(x_axis_lisbon)) {
  text(x = x_axis_lisbon[i], y = variances_lisbon[i], labels = names(variances_lisbon)[i], pos = 4, col = "black")
}

"""
Para questões de plot de thresholds

segments(x0 = 0, y0 = 0.99*max(variances_lisbon), x1 = 4, col = 'red', lty = 'dashed', lwd = 2)
text(paste0(0.99 * 100, ""), x=4, y=0.95*max(variances_lisbon)+4000)
segments(x0 = 4, y0 = 0.99*max(variances_lisbon), y1 = 0, col='red', lty = 'dashed', lwd = 2)

segments(x0 = 0, y0 = 0.85*max(variances_lisbon), x1 = 3, col = 'blue', lty = 'dashed', lwd = 2)
text(paste0(0.85 * 100, ""), x=3, y=0.85*max(variances_lisbon)+2000)
segments(x0 = 3, y0 = 0.85*max(variances_lisbon), y1 = 0, col='blue', lty = 'dashed', lwd = 2)

segments(x0 = 0, y0 = 0.75*max(variances_lisbon), x1 = 2, col = 'darkgreen', lty = 'dashed', lwd = 2)
text(paste0(0.75 * 100, ""), x=2, y=0.75*max(variances_lisbon)+1000)
segments(x0 = 2, y0 = 0.75*max(variances_lisbon), y1 = 0, col='darkgreen', lty = 'dashed', lwd = 2)
"""

plot(x = x_axis_lisbon, y = mean_median_lisbon,
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevância das features com base na média-mediana [Lisbon]", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(x_axis_lisbon)),
     ylim = c(0, max(mean_median_lisbon) + 0.1 * max(mean_median_lisbon))
)
grid(nx = NULL, ny = NULL,
      lty = 2,      # Grid line type
      col = "gray", # Grid line color
      lwd = 2)      # Grid line width
for (i in 1:length(x_axis_lisbon)) {
  text(x = x_axis_lisbon[i], y = mean_median_lisbon[i], labels = names(mean_median_lisbon)[i], pos = 4, col = "black")
}

plot(x = x_axis_pima, y = variances_pima,
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevância das features com base na variância [Pima]", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(x_axis_pima)),
     ylim = c(0, max(variances_pima) + 0.1 * max(variances_pima))
)
grid(nx = NULL, ny = NULL,
      lty = 2,      # Grid line type
      col = "gray", # Grid line color
      lwd = 2)      # Grid line width
for (i in 1:length(x_axis_lisbon)) {
  text(x = x_axis_pima[i], y = variances_pima[i], labels = names(variances_pima)[i], pos = 4, col = "black")
}


plot(x = x_axis_pima, y = mean_median_pima,
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevância das features com base na média-mediana [Pima]", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(x_axis_pima)),
     ylim = c(0, max(mean_median_pima) + 0.1 * max(mean_median_pima))
)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width
for (i in 1:length(x_axis_lisbon)) {
  text(x = x_axis_pima[i], y = mean_median_pima[i], labels = names(mean_median_pima)[i], pos = 4, col = "black")
}

# ------------------------------------------------------------------------------------

# Alinea (b)
thresholds <- c(0.75, 0.85, 0.95) # Example thresholds
adequate_features <- rep(0, length(thresholds))


for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(variances_lisbon) < threshold * sum(variances_lisbon)) + 1
  adequate_features[i] <- m
  # Get the names of the features
  feature_names <- names(variances_lisbon)[1:m]
  cat("For values of variances threshold", threshold * 100, "%", "we need features:", paste(feature_names, collapse = ", "), "\n")
}

thresholds_lisbon = data.frame(paste(thresholds * 100, "%"), adequate_features)
colnames(thresholds_lisbon) <- c("Threshold [%]", "Features adequadas")
View(thresholds_lisbon)

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(mean_median_lisbon) < threshold * sum(mean_median_lisbon)) + 1
  adequate_features[i] <- m
  # Get the names of the features
  feature_names <- names(variances_lisbon)[1:m]
  cat("For values of mean-median threshold", threshold * 100, "%", " we need features:", paste(feature_names, collapse = ", "), "\n")
}

thresholds_lisbon = data.frame(paste(thresholds * 100, "%"), adequate_features)
colnames(thresholds_lisbon) <- c("Threshold [%]", "Features adequadas")
View(thresholds_lisbon)


for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(variances_pima) < threshold * sum(variances_pima)) + 1
  adequate_features[i] <- m
  # Get the names of the features
  feature_names <- names(variances_pima)[1:m]
  cat("For values of variances threshold", threshold * 100, "%", " we need features:", paste(feature_names, collapse = ", "), "\n")
}

thresholds_pima = data.frame(paste(thresholds * 100, "%"), adequate_features)
colnames(thresholds_pima) <- c("Threshold [%]", "Features adequadas")
View(thresholds_pima)

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(mean_median_pima) < threshold * sum(mean_median_pima)) + 1
  adequate_features[i] <- m
  # Get the names of the features
  feature_names <- names(mean_median_pima)[1:m]
  cat("For values of mean-median threshold", threshold * 100, "%", " we need features:", paste(feature_names, collapse = ", "), "\n")
}

thresholds_pima = data.frame(paste(thresholds * 100, "%"), adequate_features)
colnames(thresholds_pima) <- c("Threshold [%]", "Features adequadas")
View(thresholds_pima)

# Alinea (c)

# Considerando como class label as condições temporais para o dataset de lisboa
# E a feature "Class" do dataset pima, podemos determinar os fishers ratio.

# Means of lisbon and pima classes
lisbon_class_means = getClassMeans(data_lisbon, "conditions")

# Icon produces de same result
# lisbon_class_means = getClassMeans(data_lisbon, "icon")

pima_class_means = getClassMeans(data_pima, "Class")

# Variances of lisbon and pima classes
lisbon_class_vars = getClassVars(data_lisbon, "conditions")

# Icon produces de same result
# lisbon_class_vars = getClassVars(data_lisbon, "icon")

pima_class_vars = getClassVars(data_pima, "Class")



# Compute fishers ratio
# Squared subtraction of the means of each class divided by the sum of the variance of each class
# All this is done for all attributes
# Therefore there will be a Fishers Ratio for each attribute in the dataset
lisbon_fishers_ratio_numerator = apply(lisbon_class_means, MARGIN = 1, function(x) (x[1] - sum(x[-1]))^2)
pima_fishers_ratio_numerator = apply(pima_class_means, MARGIN = 1, function(x) (x[1] - sum(x[-1]))^2)

lisbon_fishers_ratio_denominator =  apply(lisbon_class_vars, MARGIN = 1, sum)
pima_fishers_ratio_denominator = apply(pima_class_vars, MARGIN = 1, sum)


fishers_ratio_lisbon = lisbon_fishers_ratio_numerator / lisbon_fishers_ratio_denominator
fishers_ratio_pima = pima_fishers_ratio_numerator / pima_fishers_ratio_denominator

# Replace Inf and NaN values with 0
fishers_ratio_lisbon[!is.finite(fishers_ratio_lisbon)] <- 0
fishers_ratio_pima[!is.finite(fishers_ratio_pima)] <- 0

# Quick summary of fishers ratio
fishers_ratio_lisbon <- sort(fishers_ratio_lisbon, decreasing = TRUE)
fishers_ratio_pima <- sort(fishers_ratio_pima, decreasing = TRUE)

# Plot the features

par(mfrow = c(1,2))

plot(x = x_axis_lisbon, y = fishers_ratio_lisbon,
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevância das features com base na média-mediana [Lisbon]", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(x_axis_lisbon)),
     ylim = c(0, max(fishers_ratio_lisbon) + 0.1 * max(fishers_ratio_lisbon))
)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width
for (i in 1:length(x_axis_lisbon)) {
  text(x = x_axis_lisbon[i], y = fishers_ratio_lisbon[i], labels = names(fishers_ratio_lisbon)[i], pos = 4, col = "black")
}

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(fishers_ratio_lisbon) < threshold * sum(fishers_ratio_lisbon)) + 1
  adequate_features[i] <- m
  # Get the names of the features
  feature_names <- names(fishers_ratio_lisbon)[1:m]
  cat("For values of FiRi threshold", threshold * 100, "%", " we need features:", paste(feature_names, collapse = ", "), "\n")
}

thresholds_lisbon = data.frame(paste(thresholds * 100, "%"), adequate_features)
colnames(thresholds_lisbon) <- c("Threshold [%]", "Features adequadas")
View(thresholds_lisbon)

plot(x = x_axis_pima, y = fishers_ratio_pima,
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevância das features com base na média-mediana [Lisbon]", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(x_axis_pima)),
     ylim = c(0, max(fishers_ratio_pima) + 0.1 * max(fishers_ratio_pima))
)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width
for (i in 1:length(x_axis_pima)) {
  text(x = x_axis_pima[i], y = fishers_ratio_pima[i], labels = names(fishers_ratio_pima)[i], pos = 4, col = "black")
}

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(fishers_ratio_pima) < threshold * sum(fishers_ratio_pima)) + 1
  adequate_features[i] <- m
  # Get the names of the features
  feature_names <- names(fishers_ratio_pima)[1:m]
  cat("For values of FiRi threshold", threshold * 100, "%", " we need features:", paste(feature_names, collapse = ", "), "\n")
}

thresholds_pima = data.frame(paste(thresholds * 100, "%"), adequate_features)
colnames(thresholds_pima) <- c("Threshold [%]", "Features adequadas")
View(thresholds_pima)




