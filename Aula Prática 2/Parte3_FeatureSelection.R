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

"""
convertQualitativeFeatures = function(dataset, qualitative_features){
  
  categorical_features = getCategoricalFeatures(dataset)
  
  for(feature in categorical_features){
    # Determine different categorical values in feature
    categorical_types_of_feature = unique(dataset[[feature]])
    print(categorical_types_of_feature)
    
    # Determine occurrences of each values
    abs_freq = sapply(categorical_types_of_feature, function(categorical_types_of_feature) sum(dataset[[feature]] == categorical_types_of_feature))
    # Calculate relative frequency of value
    relative_freq = round(abs_freq / length(dataset[[feature]]), digits = 3)
    
    # Replace categorical feature with equivalent relative frequency
    dataset[[feature]] = relative_freq
    
  }
  return(dataset)
}"""

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

data_lisbon = Lisbon_.2023.01.01_2023.01.31
data_pima = pima

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


# ------------------------------------------------------------------------------------

# Alinea (b)
thresholds <- c(0.75, 0.85, 0.99) # Example thresholds
adequate_features <- rep(0, length(thresholds))

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(variances_lisbon) < threshold * sum(variances_lisbon)) + 1
  adequate_features[i] <- m
  cat("For threshold", threshold, "the number of adequate features in Lisbon dataset is:", m, "\n")
}

thresholds_lisbon = data.frame(paste(thresholds * 100, "%"), adequate_features)
colnames(thresholds_lisbon) <- c("Threshold [%]", "Features adequadas")
#View(thresholds_lisbon)

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(variances_pima) < threshold * sum(variances_pima)) + 1
  adequate_features[i] <- m
  cat("For threshold", threshold, "the number of adequate features in Pima dataset is:", m, "\n")
}

thresholds_pima = data.frame(paste(thresholds * 100, "%"), adequate_features)
colnames(thresholds_pima) <- c("Threshold [%]", "Features adequadas")
#View(thresholds_pima)

# Alinea (c)

# Considerando como class label as condições temporais para o dataset de lisboa
# E a feature "Class" do dataset pima, podemos determinar os fishers ratio.

# Means of lisbon and pima classes
lisbon_class_means = getClassMeans(data_lisbon, "conditions")
pima_class_means = getClassMeans(data_pima, "Class")

# Variances of lisbon and pima classes
lisbon_class_vars = getClassVars(data_lisbon, "conditions")
pima_class_vars = getClassVars(data_pima, "Class")



# Compute fishers ratio
# Squared subtraction of the means of each class divided by the sum of the variance of each class
# All this is done for all attributes
# Therefore there will be a Fishers Ratio for each attribute in the dataset
lisbon_fishers_ratio_numerator = (-1*sum(lisbon_class_means))^2
lisbon_fishers_ratio_denominator = sum(lisbon_class_vars)^2


pima_fishers_ratio_numerator = (-1*sum(pima_class_means))^2
pima_fishers_ratio_denominator = sum(pima_class_vars)^2

fishers_ratio_lisbon = lisbon_fishers_ratio_numerator / lisbon_fishers_ratio_denominator
fishers_ratio_pima = pima_fishers_ratio_numerator / pima_fishers_ratio_denominator







