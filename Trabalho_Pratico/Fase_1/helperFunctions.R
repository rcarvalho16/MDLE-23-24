getCategoricalFeatures = function(dataset){
  feature_types = sapply(dataset, class)
  names(feature_types)[!(feature_types %in% c("numeric", "integer"))]
}

getClassMeans = function(dataset, classFeature){
  labels_feature = unique(dataset[[classFeature]])
  res <- sapply(labels_feature, function(label) round(colMeans(dataset[dataset[[classFeature]] == label,]), digits = 2))
  
  # Remove label from mean, as it does not matter in the variance calculator
  label_index <- grep(classFeature, colnames(dataset))
  res <- res[-label_index,]
  return(res)
}

getClassVars = function(dataset, classFeature){
  labels_feature = unique(dataset[[classFeature]])
  res <- sapply(labels_feature, function(label) round(sapply(dataset[dataset[[classFeature]] == label,], var), digits = 2))
  
  # Remove label from vars, as it does not matter in the variance calculator
  label_index <- grep(classFeature, colnames(dataset))
  res <- res[-label_index,]
  return(res)
}

readWeatherFun <- function(i) { #read CSV data
  read.csv(paste(weather_path,i,sep = "/"), header=TRUE,stringsAsFactors = FALSE)
}

readEnergyFun <- function(i) { #read CSV data
  read.csv(paste(data_path,i,sep = "/"), header=TRUE,stringsAsFactors = FALSE, sep = ";")
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

calculate_summary <- function(data) {
  summary_df <- data.frame(variable = character(), mean = numeric(), sd = numeric(), stringsAsFactors = FALSE)
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      mean_val <- mean(data[[col]], na.rm = TRUE)
      sd_val <- sd(data[[col]], na.rm = TRUE)
      summary_df <- rbind(summary_df, data.frame(variable = col, mean = mean_val, sd = sd_val))
    }
  }
  return(summary_df)
}

VarianceThresholdFeatureSelection = function(dataset, threshold){
  dataset_original <- dataset

  # Step 1: Convert all the qualitative features into numerical
  dataset <- convertQualitativeFeatures(dataset)

  # Step 2: Normalize the data
  dataset <- apply(dataset, 2, function(column) column/max(column))

  # Step 3: Calculate the variance of each feature
  variances_dataset <- apply(dataset, 2, var)

  # Sort variances
  variances_dataset <- sort(variances_dataset, decreasing = TRUE)

  # Step 3: Calculate the minimum number of features with a cumulative sum of variance of at least threshold value
  cumulative_sum_variance_above_threshold <- cumsum(variances_dataset) >= threshold

  last_relevant_feature <- which(cumulative_sum_variance_above_threshold == TRUE)[1]
  
  relevant_features <- names(variances_dataset[1:last_relevant_feature])

  return(as.data.frame(dataset_original[,relevant_features]))
}

FisherRatioFeatureSelection = function(dataset, class_label, threshold){
  dataset_original <- dataset
  
  dataset <- convertQualitativeFeatures(dataset)
  
  # Normalize the data
  dataset <- apply(dataset, 2, function(column) column/max(column))
  
  dataset <- as.data.frame(dataset)
  
  dataset_class_means = getClassMeans(dataset, class_label)
  
  dataset_class_vars = getClassVars(dataset, class_label)
  
  # Compute fishers ratio
  # Squared subtraction of the means of each class divided by the sum of the variance of each class
  # All this is done for all attributes
  # Therefore there will be a Fishers Ratio for each attribute in the dataset
  dataset_fishers_ratio_numerator = apply(dataset_class_means, MARGIN = 1, function(x) (x[1] - sum(x[-1]))^2)
  
  dataset_fishers_ratio_denominator =  apply(dataset_class_vars, MARGIN = 1, sum)
  
  fishers_ratio_dataset = dataset_fishers_ratio_numerator / dataset_fishers_ratio_denominator
  
  # Replace Inf and NaN values with 0
  fishers_ratio_dataset[!is.finite(fishers_ratio_dataset)] <- 0
  
  # Quick summary of fishers ratio
  fishers_ratio_dataset <- sort(fishers_ratio_dataset, decreasing = TRUE)
  threshold_fishers_ratio <- sum(fishers_ratio_dataset) * threshold
  
  fisher_ratio_cumulative_sum <- cumsum(fishers_ratio_dataset)
  
  last_relevant_feature <- which((fisher_ratio_cumulative_sum >= threshold_fishers_ratio) == TRUE)[1]
  
  # The features with a relevance above 0 are stored
  relevant_features <- c(names(fishers_ratio_dataset[1:last_relevant_feature]), class_label)
  
  return(as.data.frame(dataset_original[,relevant_features]))
}

convertTimestamps = function(dataset){
  
  # Add day of the week
  dataset$Day_of_Week <- wday(dataset$Date, label = TRUE, abbr = FALSE)
  
  # Convert date into different columns
  dates <- as.Date(dataset$Date)
  dataset$Year <- as.integer(format(dates, "%Y"))
  dataset$Month <- as.integer(format(dates, "%m"))
  dataset$Day <- as.integer(format(dates, "%e"))
  dataset$Hours <- hour(as.POSIXct(dataset$Hour, format = "%H:%M"))
  rm(dates)
  
  # Remove redundant date columns
  #dataset <- dataset[, -c(1,2,3)]
  remove <- c("Date.Time", "Date", "Hour", "datetime")
  dataset <- dataset[, !names(dataset) %in% remove]
  
  # Reorder columns
  order <- unique(c("Day", "Month", "Year", "Hours", "Day_of_Week", names(dataset)))
  dataset <- dataset[, order]
  #dataset <- dataset[, c(6,5,4,7,3,2,1)]
  
  return(dataset)
}

equalFrequencyBinning = function(data){
  if(class(data) %in% c("numeric", "integer")){
    n_bins <- ceiling(log(length(unique(data)), 2)+1)
    if(n_bins == 0){
      next
    }
    bin_breaks <- quantile(data, probs = seq(0,1,1/n_bins)[-1])
    bin_breaks <- unique(bin_breaks)
    if(length(bin_breaks) <= 1){
      next
    }
    bin_breaks <- c(-Inf, bin_breaks)
    data <- cut(data, breaks = bin_breaks)
  }
  
  return(data)
}
