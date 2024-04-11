# IMPORTS
library(dplyr)
library(lubridate)

# AUX FUNCTIONS
######################
if(!exists("getCategoricalFeatures", mode="function")) 
  source("helperfunctions.R")
#########################

# Prepare data path
data_path <- "data"
weather_path <- paste(data_path,"Weather", sep = "/")

# Read files
energy_data <- c(list.files(data_path, pattern = ".csv"))
weather_data <- c(list.files(weather_path, pattern = ".csv"))
weather_data <- do.call(rbind, lapply(weather_data, readWeatherFun ))
energy_data <- do.call(rbind, lapply(energy_data, readEnergyFun ))

# Convert to dataframes
weather_data <- as.data.frame(weather_data)
energy_data <- as.data.frame(energy_data)

# Check empty rows in weather dataset
apply(weather_data, MARGIN = 2, function(col) sum(is.na(col)))

# Check SD and mean of each feature
calculate_summary(weather_data)

# Impute missing data
# This will be a topic where we need further opinions given the bias introduced by imputing based on the mean values.
# REF for preciptype imputation: https://www.visualcrossing.com/resources/documentation/weather-data/weather-data-documentation/
weather_data$preciptype[is.na(weather_data$preciptype) | weather_data$preciptype == ""] <- "unknown"
weather_data$solarradiation[is.na(weather_data$solarradiation) | weather_data$solarradiation == ""] <- round(median(weather_data$solarradiation, na.rm = TRUE))
weather_data$solarenergy[is.na(weather_data$solarenergy) | weather_data$solarenergy == ""] <- mean(as.integer(weather_data$solarenergy), na.rm = TRUE)
weather_data$uvindex[is.na(weather_data$uvindex) | weather_data$uvindex == ""] <- round(median(weather_data$uvindex, na.rm = TRUE))
weather_data$severerisk[is.na(weather_data$severerisk) | weather_data$severerisk == ""] <- 0

# Check empty rows in weather dataset
apply(weather_data, MARGIN = 2, function(col) sum(is.na(col)))

# Get only lisbon power consumption
energy_data <- energy_data[energy_data$Zip.Code <= 1999,]

# 1 - Convert Timestamps to same format
# Take Date and Hour features and join them in the same format as weather dataset
energy_data$Date.Time <- apply(
  energy_data,
  MARGIN = 1,
  function(row) paste(row["Date"], "T", row["Hour"], ":00", sep = "")
)

########################################################
# Check the influence of weather on the energy consumption

lisbon_zip_code <- 1000
class_label <- "conditions"
fisher_ratio_threshold <- 0.95
  
# Obtain the energy data of Lisbon
lisbon_zipcode_energy_data <- energy_data[energy_data$Zip.Code == lisbon_zip_code, ]

# Relate the energy data with the weather data of Lisbon
lisbon_zipcode_consumption <- merge(weather_data, lisbon_zipcode_energy_data, by.x = "datetime", by.y = "Date.Time")

# Divide the timestamps into multiple columns to check if certain members of
# the date (day, month or year) could also influence the consumption
lisbon_zipcode_consumption <- convertTimestamps(lisbon_zipcode_consumption)

# Feature Selection - Fisher's Ratio
lisbon_zipcode_consumption_fisher_ratio <- FisherRatioFeatureSelection(lisbon_zipcode_consumption, class_label, fisher_ratio_threshold)
