data = Lisbon_.2023.01.01_2023.01.31

# Step 1: Calculate the variance of each feature
variances <- sapply(data, var)
# Replace NA values with 0
variances[is.na(variances)] = 0

# Step 2: Compute the mean and median of each feature
means <- sapply(data, mean)
# Replace NA values with 0
means[is.na(means)] = 0

medians <- sapply(data, median)
# Replace NA values with 0
medians[is.na(medians)] = 0

# Step 3: Compute the absolute difference between the mean and median
abs_diff <- abs(means - medians)


# Step 4: Combine variance and absolute difference as relevance measures
relevance_measures <- variances * abs_diff
relevance_measures <- sort(relevance_measures, decreasing = TRUE)


# Step 5: Plot it
feature_names <- colnames(data)
x_axis <- seq_along(feature_names)


# Alinea (a)
plot(x = x_axis, y = relevance_measures, 
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevance Measures of Features", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(feature_names)),
     ylim = c(0, max(relevance_measures) + 0.1 * max(relevance_measures))
     )

# Alinea (b)
# Btw isto não deve estar certo lmao
thresholds <- c(0.75, 0.85, 0.95) # Example thresholds
adequate_features <- rep(0, length(thresholds))

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  m <- sum(cumsum(variances) < threshold * sum(variances)) + 1
  adequate_features[i] <- m
  cat("For threshold", threshold, "the number of adequate features is:", m, "\n")
}


