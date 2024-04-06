# IMPORTS
library(dplyr)
library(ggplot2)

# AUX FUNCTIONS
######################
readWeatherFun <- function(i) { #read CSV data
  read.csv(paste(weather_path,i,sep = "/"), header=TRUE,stringsAsFactors = FALSE)
}

readEnergyFun <- function(i) { #read CSV data
  read.csv(paste(data_path,i,sep = "/"), header=TRUE,stringsAsFactors = FALSE, sep = ";")
}

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

# Get only lisbon power consumption
# Zip code format is 1xxx
energy_data <- energy_data[energy_data$Zip.Code <= 1999,]

# 1 - Convert Timestamps to same format
# Take Date and Hour features and join them in the same format as weather dataset
energy_data$Date.Time <- apply(
  energy_data,
  MARGIN = 1,
  function(row) paste(row["Date"], "T", row["Hour"], ":00", sep = "")
)


# Plot the power consumption distribution in Lisbon, per hour
mean_energy <- energy_data %>%
  group_by(Date.Time, Hour) %>%
  summarise(mean_energy = mean(Active.Energy..kWh.)) %>%
  group_by(Hour) %>%
  summarise(mean_energy = mean(mean_energy))


plot.ts(mean_energy$mean_energy, main = "Mean Energy Consumption per Hour from Dec2022 - Oct2023",
        xlab = "Hour",
        ylab = "Mean Energy Consumption [KW/h]")
grid(lty = 3)  # Add a thin grid


# Join power consumption dataset to weather dataset by timestamp
# Discard redundant columns (ie: Timestamp on both datasets is equal)
lisbon_consumption <- merge(weather_data, energy_data, by.x = "datetime", by.y = "Date.Time")

# Compute mean energetic consumption per timestamp in Lisbon City

# 1 - Remove ZIP Code and Redundant Lisbon tag
remove <- c(2, ncol(lisbon_consumption) - 1)
lisbon_consumption <- lisbon_consumption[,-remove]

# 2 - Group by timestamp, calculating the mean of Energetic Consumption
lisbon_consumption <- lisbon_consumption %>%
  group_by(datetime) %>%
  mutate(Active.Energy..kWh. = mean(Active.Energy..kWh.))

# 3 - Remove duplicate entries by using unique()
lisbon_consumption <- unique(lisbon_consumption)



