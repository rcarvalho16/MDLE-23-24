library(dplyr)
library(ggplot2)
library(lubridate)

# AUX FUNCTIONS
######################
readEnergyFun <- function(i) { #read CSV data
  read.csv(paste(data_path,i,sep = "/"), header=TRUE,stringsAsFactors = FALSE, sep = ";")
}

if(!exists("getCategoricalFeatures", mode="function")) 
  source("helperfunctions.R")
#########################

# Main objective is to engineer data to train a classifier
# To identify Zip codes as Residential or Non-Residential
# Given their energy consumption

data_path <- "data"
energy_data <- do.call(rbind, lapply(energy_data, readEnergyFun ))
energy_data <- as.data.frame(energy_data)



# Perform analysis on Residential vs Non-Residential ZIP codes
# This classification was done by hand. Prone to human-error

# Examples of residential Zip Codes:
# These will be classified as Residential
# Massama e Monte Abraao (2745), Avenida da Igreja (1700), Carnide (1600), Cacém (2735)
energy_data_res <- energy_data[energy_data$Zip.Code %in% c(2745, 1700, 1600, 2735), ]

# Add day of the week
energy_data_res$Day_of_Week <- wday(energy_data_res$Date, label = TRUE, abbr = FALSE)


average_consumption_weekday <- energy_data_res %>%
  group_by(Zip.Code, Day_of_Week) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Compare average consumption per hour
average_consumption_hour <- energy_data_res %>%
  group_by(Zip.Code, Hour) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Plot the average weekday consumption bar chart
ggplot(average_consumption_weekday, aes(x = Day_of_Week, y = Average_Consumption, fill = factor(Zip.Code))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Consumption by Day of the Week and Zip Code",
       x = "Day of the Week",
       y = "Average Consumption (kWh)",
       fill = "Zip Code") +
  theme_minimal()


# Plot the line chart
ggplot(average_consumption_hour, aes(x = Hour, y = Average_Consumption, color = factor(Zip.Code))) +
  geom_line() +
  labs(title = "Average Consumption by Hour and Zip Code",
       x = "Hour of the Day",
       y = "Average Consumption (kWh)",
       color = "Zip Code") +
  theme_minimal()


# Examples of non residential Zip Codes:
# Industrial sites, these will be classified as Non-Residential
# Morlena, Sines (7520), Torres Novas (2350), Palmela (2950) 

energy_data_res <- energy_data[energy_data$Zip.Code %in% c(2745, 1700, 1600, 2735), ]

# Add day of the week
energy_data_res$Day_of_Week <- wday(energy_data_res$Date, label = TRUE, abbr = FALSE)