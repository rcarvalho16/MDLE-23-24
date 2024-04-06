# IMPORTS
library(dplyr)
library(ggplot2)
library(gridExtra)

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

#######################################################
# Compute mean energetic consumption per timestamp in Lisbon City

# Choose a list of 3 residential zip codes and 3 industrial zip codes
# to study how the weather influences the energy consumption
#energy_data_res_ind <- energy_data[energy_data$Zip.Code %in% c(1700, 1600, 7520, 2350, 2950), ]
energy_data_res_ind <- energy_data[energy_data$Zip.Code %in% c(1700), ]

# Join power consumption dataset to weather dataset by timestamp
# Discard redundant columns (ie: Timestamp on both datasets is equal)
lisbon_consumption <- merge(weather_data, energy_data_res_ind, by.x = "datetime", by.y = "Date.Time")

# 1 - Remove ZIP Code and Redundant Lisbon tag
remove <- c(2, ncol(lisbon_consumption) - 1)
lisbon_consumption <- lisbon_consumption[,-remove]

# 2 - Group by timestamp, calculating the mean of Energetic Consumption
lisbon_consumption <- lisbon_consumption %>%
  group_by(datetime) %>%
  mutate(Active.Energy..kWh. = mean(Active.Energy..kWh.))

# 3 - Remove duplicate entries by using unique()
lisbon_consumption <- unique(lisbon_consumption

########################################################
# Check the influence of weather on the energy consumption

zip_codes <- sample(energy_data$Zip.Code, 6)
#c(1700, 1600, 1495, 1900, 1350, 1675)

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
