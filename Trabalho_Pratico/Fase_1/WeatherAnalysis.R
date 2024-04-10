# IMPORTS
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
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
# energy_data_lisbon <- [energy_data$Zip.Code == 1000,]

# 1 - Convert Timestamps to same format
# Take Date and Hour features and join them in the same format as weather dataset
energy_data$Date.Time <- apply(
  energy_data,
  MARGIN = 1,
  function(row) paste(row["Date"], "T", row["Hour"], ":00", sep = "")
)

#energy_data_lisbon$Date.Time <- apply(
#  energy_data,
#  MARGIN = 1,
#  function(row) paste(row["Date"], "T", row["Hour"], ":00", sep = "")
#)

#######################################################
# Study how the weather affects the energy consumption for a certain zip code
lisbon_zip_code <- 1000

# Obtain only the energy data related to the chosen zip code
lisbon_zipcode_energy_data <- energy_data[energy_data$Zip.Code == lisbon_zip_code, ]

# Merge the weather data with energy data
lisbon_zipcode_consumption <- merge(weather_data, lisbon_zipcode_energy_data, by.x = "datetime", by.y = "Date.Time")

# Add the day of week based on the date
lisbon_zipcode_consumption$Day_of_Week <- wday(lisbon_zipcode_consumption$Date, label = TRUE, abbr = FALSE)

# Remove redundant columns such as Zip.Code, name, datetime, stations, icon
# Transform Date to Day, Month, Year, Hour
# We can use conditions as a class label


# Remove redundant Datetime, Lisbon tag, Zip code, stations, icons
remove <- c(1,2,23,24,27)
lisbon_zipcode_consumption <- lisbon_zipcode_consumption[,-remove]

# Transform Date to Day Month Year Hour (minutes are always 00 so no need)

# Normalize the energy consumption
lisbon_zipcode_consumption$Active.Energy..kWh. <- lisbon_zipcode_consumption$Active.Energy..kWh./max(lisbon_zipcode_consumption$Active.Energy..kWh.)

# Plot the average conditions consumption per Zip.Code bar chart
ggplot(lisbon_zipcode_consumption, aes(x = conditions, y = Active.Energy..kWh., fill = factor(Day_of_Week))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Residential Consumption by conditions and for Zip Code", lisbon_zip_code),
       x = "conditions",
       y = "Average Consumption (kWh)",
       fill = "Day of Week",
       margin = element_text()) +
  theme_minimal()

########################################################
# Check the influence of weather on the energy consumption
  
# Obtain the energy data of lisbon
lisbon_zipcode_energy_data <- energy_data[energy_data$Zip.Code %in% lisbon_zip_code, ]

lisbon_zipcode_consumption <- merge(weather_data, lisbon_zipcode_energy_data, by.x = "datetime", by.y = "Date.Time")
  
# undersample the consumptions when its raining
rain_conditions_lisbon_consumption <- lisbon_zipcode_consumption[lisbon_zipcode_consumption$conditions %in% c("Rain, Overcast", "Rain, Partially cloudy"), ]
rain_conditions_lisbon_consumption$conditions <- "Rain"

# undersample the consumptions when the weather is clear and the hours match with the hours when its raining
clear_conditions_lisbon_consumption <- lisbon_zipcode_consumption[lisbon_zipcode_consumption$conditions %in% c("Clear") & lisbon_zipcode_consumption$Hour %in% rain_conditions_lisbon_consumption$Hour, ]
  
# Remove ZIP Code and Redundant Lisbon tag
#remove <- c(2, ncol(rain_conditions_lisbon_consumption) - 1)
#rain_conditions_lisbon_consumption <- rain_conditions_lisbon_consumption[,-remove]
#remove <- c(2, ncol(clear_conditions_lisbon_consumption) - 1)
#clear_conditions_lisbon_consumption <- clear_conditions_lisbon_consumption[,-remove]
  
# Compute the mean consumption per hour
rain_conditions_lisbon_consumption <- rain_conditions_lisbon_consumption %>%
  group_by(Hour, conditions) %>%
  summarize(Active.Energy..kWh. = mean(Active.Energy..kWh.))

clear_conditions_lisbon_consumption <- clear_conditions_lisbon_consumption %>%
  group_by(Hour, conditions) %>%
  summarize(Active.Energy..kWh. = mean(Active.Energy..kWh.))
  
# Merge the raining table with the clear weather table
extreme_conditions_lisbon_consumption <- merge(rain_conditions_lisbon_consumption, clear_conditions_lisbon_consumption, all = TRUE)

# Plot the line chart
ggplot(extreme_conditions_lisbon_consumption, aes(x = Hour, y = Active.Energy..kWh., color = factor(conditions))) +
  geom_line(aes(group = conditions)) +
  labs(title = paste("Average Consumption by Hour and Weather Condition of zip code", lisbon_zip_code),
       x = "Hour of the Day",
       y = "Average Consumption (kWh)",
       color = "Weather Conditions") +
  theme_minimal()
