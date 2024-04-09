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