################### AUX FUNCTIONS #############################
convertQualitativeFeatures = function(dataset, qualitative_features){
  
  for(feature in qualitative_features){
    dataset[feature] = factor(dataset[[feature]], labels = seq(1, length(unique(factor(dataset[[feature]])))))
  }
  return(dataset)
}
###############################################################

data_lisbon = Lisbon_.2023.01.01_2023.01.31
data_pima = pima

# Preparing datasets
qualitative_features_pima = c("age","menopause", "tumor.size", "inv.nodes", "node.caps",
                              "breast", "breast.quad","irradiat", "Class")

data_pima = convertQualitativeFeatures(data_pima, qualitative_features_pima)


# Alinea (a)

# Step 1: Calculate the variance of each feature
variances_lisbon <- sapply(data_lisbon, var)
variances_pima <- sapply(data_pima, var)
# Replace NA values with 0
variances_lisbon[is.na(variances_lisbon)] = 0
variances_pima[is.na(variances_pima)] = 0

# Step 2: Compute the mean and median of each feature
means_lisbon <- sapply(data_lisbon, mean)
means_pima <- sapply(data_pima, mean)
# Replace NA values with 0
means_lisbon[is.na(means_lisbon)] = 0
means_pima[is.na(means_pima)] = 0

medians_lisbon <- sapply(data_lisbon, median)
medians_pima <- sapply(data_pima, median)
# Replace NA values with 0
medians_lisbon[is.na(medians_lisbon)] = 0
medians_pima[is.na(medians_pima)] = 0

# Step 3: Compute the absolute difference between the mean and median
mean_median_lisbon <- abs(means_lisbon - medians_lisbon)
mean_median_pima <- abs(means_pima - medians_pima)


# Step 4: Combine variance and absolute difference as relevance measures
relevance_measures_lisbon <- variances_lisbon * mean_median_lisbon
relevance_measures_pima <- variances_pima * mean_median_pima

relevance_measures_lisbon <- sort(relevance_measures_lisbon, decreasing = TRUE)
relevance_measures_pima <- sort(relevance_measures_pima, decreasing = TRUE)


# Step 5: Plot it
x_axis_lisbon <- as.numeric(seq_along(data_lisbon))
x_axis_pima <- as.numeric(seq_along(data_pima))


par(mfrow = c(1,2))


plot(x = x_axis_lisbon, y = relevance_measures_lisbon,
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevância das features [Lisbon]", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(x_axis_lisbon)),
     ylim = c(0, max(relevance_measures_lisbon) + 0.1 * max(relevance_measures_lisbon))
     )

plot(x = x_axis_pima, y = relevance_measures_pima,
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevância das features [Pima]", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(x_axis_pima)),
     ylim = c(0, max(relevance_measures_pima) + 0.1 * max(relevance_measures_pima))
)

# ------------------------------------------------------------------------------------

# Alinea (b)
# Btw isto não deve estar certo lmao
thresholds <- c(0.75, 0.85, 0.999999) # Example thresholds
adequate_features <- rep(0, length(thresholds))

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(variances_lisbon) < threshold * sum(variances_lisbon)) + 1
  adequate_features[i] <- m
  cat("For threshold", threshold, "the number of adequate features in Lisbon dataset is:", m, "\n")
}

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(variances_pima) < threshold * sum(variances_pima)) + 1
  adequate_features[i] <- m
  cat("For threshold", threshold, "the number of adequate features in Pima dataset is:", m, "\n")
}

# Alinea (c)

# For Lisbon Dataset, we consider the conditions to be the classifier
# So there are 4 different classes: 'Overcast', 'Partially cloudy'; 'Rain, Partially cloudy'; 'Clear'

# For Pima dataset we have a feature

calculateFeatureMean = function (dataset, classLabel, key){
  return(sapply(dataset[dataset[[classLabel]] == key,],mean))
}

calculateFeatureVariance = function (dataset, classLabel, key){
  return(sapply(dataset[dataset[[classLabel]] == key,],var))
}
# Epa somehow não consigo meter isto numa espécie de array ou qualquer coisa

# Means of lisbon classes
mean_lisbon_overcast <- calculateFeatureMean(data_lisbon, "conditions", "Overcast")
mean_lisbon_pcloudy <- calculateFeatureMean(data_lisbon, "conditions", "Partially cloudy")
mean_lisbon_rainpcloudy <- calculateFeatureMean(data_lisbon, "conditions", "Rain, Partially cloudy")
mean_lisbon_clear <- calculateFeatureMean(data_lisbon, "conditions", "Clear")

# Means of pima classes
mean_pima_norec <- calculateFeatureMean(data_pima, "Class", "no-recurrence-events")
mean_pima_rec <- calculateFeatureMean(data_pima, "Class", "recurrence-events")


# Variances of lisbon classes
var_lisbon_overcast <- calculateFeatureVariance(data_lisbon, "conditions", "Overcast")
var_lisbon_pcloudy <- calculateFeatureVariance(data_lisbon, "conditions", "Partially cloudy")
var_lisbon_rainpcloudy <- calculateFeatureVariance(data_lisbon, "conditions", "Rain, Partially cloudy")
var_lisbon_clear <- calculateFeatureVariance(data_lisbon, "conditions", "Clear")

# Variances of pima classes
var_pima_norec <- calculateFeatureVariance(data_pima, "Class", "no-recurrence-events")
var_pima_rec <- calculateFeatureVariance(data_pima, "Class", "recurrence-events")


# Compute fishers ratio
# Squared subtraction of the means of each class divided by the sum of the variance of each class
# All this is done for all attributes
# Therefore there will be a Fishers Ratio for each attribute in the dataset
lisbon_fishers_ratio_numerator = 
  (mean_lisbon_overcast - mean_lisbon_pcloudy - mean_lisbon_rainpcloudy - mean_lisbon_clear)^2

lisbon_fishers_ratio_denominator = 
  (var_lisbon_overcast + var_lisbon_pcloudy + var_lisbon_rainpcloudy + var_lisbon_clear)^2


pima_fishers_ratio_numerator = 
  (mean_pima_norec - mean_pima_rec)^2

pima_fishers_ratio_denominator = 
  (var_pima_norec + var_pima_rec)^2

fishers_ratio_lisbon = lisbon_fishers_ratio_numerator / lisbon_fishers_ratio_denominator
fishers_ratio_pima = pima_fishers_ratio_numerator / pima_fishers_ratio_denominator







