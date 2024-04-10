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
# Compute the energetic consumption per icon in certain zip_codes in Lisbon City

# Choose a list of 3 residential zip codes and 2 industrial zip codes
# to study how the weather influences the energy consumption
res_zip_codes <- c(1000, 1600, 1700)
ind_zip_codes <- c(1300, 1250)

energy_data_res <- energy_data[energy_data$Zip.Code %in% res_zip_codes, ]
energy_data_ind <- energy_data[energy_data$Zip.Code %in% ind_zip_codes, ]

# Join power consumption dataset to weather dataset by timestamp
# Discard redundant columns (ie: Timestamp on both datasets is equal)
lisbon_consumption_res <- merge(weather_data, energy_data_res, by.x = "datetime", by.y = "Date.Time")
lisbon_consumption_ind <- merge(weather_data, energy_data_ind, by.x = "datetime", by.y = "Date.Time")
# lisbon_consumption_weather <- merge(weather_data, energy_data, by.x = "datetime", by.y = "Date.Time")

# Add day of the week
lisbon_consumption_res$Day_of_Week <- wday(lisbon_consumption_res$Date, label = TRUE, abbr = FALSE)
lisbon_consumption_ind$Day_of_Week <- wday(lisbon_consumption_ind$Date, label = TRUE, abbr = FALSE)
# lisbon_consumption_weather$Day_of_Week <- wday(lisbon_consumption_weather$Date, label = TRUE, abbr = FALSE)

# Remove redundant columns such as Zip.Code, name, datetime, stations, icon
# Transform Date to Day, Month, Year, Hour
# We can use conditions as a class label





# Remove redundant Datetime, Lisbon tag
remove <- c(1,2,23,24,27)
# remove_lisbon <- c(1,2,23,24,27)
lisbon_consumption_res <- lisbon_consumption_res[,-remove]
lisbon_consumption_ind <- lisbon_consumption_ind[,-remove]
# lisbon_consumption_weather <- lisbon_consumption_weather[,-remove_lisbon]

# Transform Date to Day Month Year Hour (minutes are always 00 so no need)

# CORRIGIR ISTO (N REMOVER NO LISBON_CONSUMPTION_RES O ZIP CODE)
# Group by conditions and Zip.Code, calculating the mean of Energetic Consumption
lisbon_consumption_res <- lisbon_consumption_res %>%
  group_by(conditions, Day_of_Week, Zip.Code) %>%
  summarize(Active.Energy..kWh. = mean(Active.Energy..kWh.))

lisbon_consumption_ind <- lisbon_consumption_ind %>%
  group_by(conditions, Day_of_Week, Zip.Code) %>%
  summarize(Active.Energy..kWh. = mean(Active.Energy..kWh.))

industrial_plot <- list()
residential_plot <- list()
for(i in 1:length(ind_zip_codes)){
  lisbon_consumption_ind_single_zip_code <<- lisbon_consumption_ind[lisbon_consumption_ind$Zip.Code %in% ind_zip_codes[i],]
  
  # Plot the average conditions consumption per Zip.Code bar chart
  barra_ind <<- ggplot(lisbon_consumption_ind_single_zip_code, aes(x = conditions, y = Active.Energy..kWh., fill = factor(Day_of_Week))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Residential Consumption by conditions and for Zip Code", ind_zip_codes[i]),
         x = "conditions",
         y = "Average Consumption (kWh)",
         fill = "Day of Week",
         margin = element_text()) +
    theme_minimal()
  
  industrial_plot[[i]] <- barra_ind
  
  rm(lisbon_consumption_ind_single_zip_code, barra_ind)
}
for(i in 1:length(res_zip_codes)){
  lisbon_consumption_res_single_day <<- lisbon_consumption_res[lisbon_consumption_res$Zip.Code %in% res_zip_codes[i],]

  barra_res <- ggplot(lisbon_consumption_res_single_day, aes(x = conditions, y = Active.Energy..kWh., fill = factor(Day_of_Week))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Industrial Consumption by conditions and for Zip Code", res_zip_codes[i]),
         x = "conditions",
         y = "Average Consumption (kWh)",
         fill = "Day of Week",
         margin = element_text()) +
    theme_minimal()
  
  residential_plot[[i]] <- barra_res
  
  rm(lisbon_consumption_res_single_day, barra_res)
}

grid.arrange(grobs = industrial_plot, nrow = length(ind_zip_codes))
grid.arrange(grobs = residential_plot, nrow = length(res_zip_codes))

########################################################
# Check the influence of weather on the energy consumption

zip_codes <- sample(energy_data$Zip.Code, 6)

multiple_zip_code_plots <- list()
for(i in 1:length(zip_codes)){
  zip_code <- zip_codes[i]
  
  # Obtain the energy data of a certain zip_code
  energy_data_res_ind <- energy_data[energy_data$Zip.Code %in% zip_code, ]
  
  lisbon_zip_code_consumption <- merge(weather_data, energy_data_res_ind, by.x = "datetime", by.y = "Date.Time")
  
  # undersample the consumptions when its raining
  rain_conditions_lisbon_consumption <- lisbon_zip_code_consumption[lisbon_zip_code_consumption$conditions %in% c("Rain, Overcast", "Rain, Partially cloudy"), ]
  rain_conditions_lisbon_consumption$conditions <- "Rain"
  
  # undersample the consumptions when the weather is clear and the hours match with the hours when its raining
  clear_conditions_lisbon_consumption <- lisbon_zip_code_consumption[lisbon_zip_code_consumption$conditions %in% c("Clear") & lisbon_zip_code_consumption$Hour %in% rain_conditions_lisbon_consumption$Hour, ]
  
  # Remove ZIP Code and Redundant Lisbon tag
  remove <- c(2, ncol(rain_conditions_lisbon_consumption) - 1)
  rain_conditions_lisbon_consumption <- rain_conditions_lisbon_consumption[,-remove]
  remove <- c(2, ncol(clear_conditions_lisbon_consumption) - 1)
  clear_conditions_lisbon_consumption <- clear_conditions_lisbon_consumption[,-remove]
  
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
  zip_code_plot <- ggplot(extreme_conditions_lisbon_consumption, aes(x = Hour, y = Active.Energy..kWh., color = factor(conditions))) +
    geom_line(aes(group = conditions)) +
    labs(title = paste("Average Consumption by Hour and Weather Condition of zip code", zip_code),
         x = "Hour of the Day",
         y = "Average Consumption (kWh)",
         color = "Weather Conditions") +
    theme_minimal()
  
  # Append the plot to the list
  multiple_zip_code_plots[[i]] <- zip_code_plot
}

grid.arrange(grobs = multiple_zip_code_plots, nrow = 3)

