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
min_value <- min(relevance_measures)
max_value <- max(relevance_measures)
relevance_measures <- (relevance_measures - min_value) / (max_value - min_value)
relevance_measures <- sort(relevance_measures, decreasing = TRUE)

# Step 5: Plot it
feature_names <- colnames(data)

x_axis <- 1:length(feature_names)

plot(x = x_axis, y = relevance_measures, 
     xlab = "Features", ylab = "Relevance Measure", 
     main = "Relevance Measures of Features", 
     pch = 19,
     col = "blue",
     xlim = c(0, length(feature_names)),
     ylim = c(0, max(relevance_measures) + 0.1 * max(relevance_measures))
     )
