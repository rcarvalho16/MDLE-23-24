# IMPORTS
library(ggplot2)
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

#######################################################
# Study how the weather affects the energy consumption for a certain zip code
lisbon_zip_code <- 1000

# Obtain only the energy data related to the chosen zip code
lisbon_zipcode_energy_data <- energy_data[energy_data$Zip.Code == lisbon_zip_code, ]

# Merge the weather data with energy data
lisbon_zipcode_consumption <- merge(weather_data, lisbon_zipcode_energy_data, by.x = "datetime", by.y = "Date.Time")

# Add the day of week based on the date
#lisbon_zipcode_consumption$Day_of_Week <- wday(lisbon_zipcode_consumption$Date, label = TRUE, abbr = FALSE)

# É PRECISO CORRIGIR ISTO!!!
# O FISHER'S RATIO RETIRA A COLUNA DOS DIAS DA SEMANA E POR ISSO NÃO DÁ PARA CRIAR A RELAÇÃO
lisbon_zipcode_days_of_week <- wday(lisbon_zipcode_consumption$Date, label = TRUE, abbr = FALSE)

# Remove redundant columns such as Zip.Code, name, datetime, stations, icon
# Transform Date to Day, Month, Year, Hour
# We can use conditions as a class label
lisbon_zipcode_consumption <- FisherRatioFeatureSelection(lisbon_zipcode_consumption, "conditions", 0.95)

# The fisher's ration removes the column of Day_of_Week so we add it after the
# Feature Selection to relate the consumption with the day of the week
lisbon_zipcode_consumption$Day_of_Week <- lisbon_zipcode_days_of_week

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
