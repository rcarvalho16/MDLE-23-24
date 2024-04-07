library(dplyr)
library(ggplot2)
library(lubridate)
library(cowplot)

# AUX FUNCTIONS
######################
if(!exists("getCategoricalFeatures", mode="function")) 
  source("helperfunctions.R")
#########################

# Main objective is to engineer data to train a classifier
# To identify Zip codes as Residential or Non-Residential
# Given their energy consumption

data_path <- "data"
energy_data <- c(list.files(data_path, pattern = ".csv"))
energy_data <- do.call(rbind, lapply(energy_data, readEnergyFun ))
energy_data <- as.data.frame(energy_data)



# Perform analysis on Residential vs Non-Residential ZIP codes
# This classification was done by hand. Prone to human-error
# Ref: https://www.pordata.pt/db/municipios/ambiente+de+consulta/tabela

# Examples of residential Zip Codes:
# These will be classified as Residential
res_zip_codes <- c(1000, 1600, 1700, 2120, 2230, 2675, 2735, 2745, 3610, 3630, 4910, 6060,
                   7350, 7630, 7700)
energy_data_res <- energy_data[energy_data$Zip.Code %in% res_zip_codes, ]

# Add day of the week
energy_data_res$Day_of_Week <- wday(energy_data_res$Date, label = TRUE, abbr = FALSE)

average_consumption_weekday <- energy_data_res %>%
  group_by(Zip.Code, Day_of_Week) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Compare average consumption per hour
average_consumption_hour <- energy_data_res %>%
  group_by(Zip.Code, Hour) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Normalize hourly consumption values
normalized_average_consumption_hour <- average_consumption_hour %>%
  group_by(Zip.Code) %>%
  mutate(Average_Consumption = Average_Consumption / max(Average_Consumption))



# Examples of non residential Zip Codes:
# Industrial sites, these will be classified as Non-Residential
ind_zip_codes <- c(7520, 2350, 4970, 2840, 3800, 6300, 2900, 3850, 3860, 2430, 3530, 7600,
                   7780, 7220, 4760)
energy_data_ind <- energy_data[energy_data$Zip.Code %in% ind_zip_codes, ]

# Add day of the week
energy_data_ind$Day_of_Week <- wday(energy_data_ind$Date, label = TRUE, abbr = FALSE)

# Compute average consumption
average_consumption_weekday_ind <- energy_data_ind %>%
  group_by(Zip.Code, Day_of_Week) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Compare average consumption per hour
average_consumption_hour_ind <- energy_data_ind %>%
  group_by(Zip.Code, Hour) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Normalize hourly consumption values
normalized_average_consumption_hour_ind <- average_consumption_hour_ind %>%
  group_by(Zip.Code) %>%
  mutate(Average_Consumption = Average_Consumption / max(Average_Consumption))




#############################################
# Compare residential consumption and industrial consumption


# Line chart for hourly consumption in residential zones
linha_res = ggplot(normalized_average_consumption_hour, aes(x = Hour, y = Average_Consumption, color = factor(Zip.Code))) +
  geom_line(aes(group = Zip.Code)) +
  ylim(0,1) +
  labs(title = "Average Consumption by Hour and Zip Code [Residential]",
       x = "Hour of the Day",
       y = "Average Consumption (kWh)",
       color = "Zip Code") +
  theme_minimal()

# Line chart for hourly consumption in industrial zones
linha_ind = ggplot(normalized_average_consumption_hour_ind, aes(x = Hour, y = Average_Consumption, color = factor(Zip.Code))) +
  geom_line(aes(group = Zip.Code)) +
  ylim(0,1) +
  labs(title = "Average Consumption by Hour and Zip Code [Industrial]",
       x = "Hour of the Day",
       y = "Average Consumption (kWh)",
       color = "Zip Code") +
  theme_minimal()

plot_grid(linha_res, linha_ind, labels = "AUTO")



# Plot the average weekday consumption bar chart
barra_res = ggplot(average_consumption_weekday, aes(x = Day_of_Week, y = Average_Consumption, fill = factor(Zip.Code))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Consumption by Day of the Week and Zip Code",
       x = "Day of the Week",
       y = "Average Consumption (kWh)",
       fill = "Zip Code") +
  theme_minimal()

# Plot the average weekday consumption bar chart
barra_ind = ggplot(average_consumption_weekday_ind, aes(x = Day_of_Week, y = Average_Consumption, fill = factor(Zip.Code))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Consumption by Day of the Week and Zip Code",
       x = "Day of the Week",
       y = "Average Consumption (kWh)",
       fill = "Zip Code") +
  theme_minimal()

plot_grid(barra_res, barra_ind, labels = "AUTO")

# Add class labels to each dataset
energy_data_ind$Zone <- "Industrial"
energy_data_res$Zone <- "Residential"


# Unite two datasets into one
energy_data_labeled <- rbind(energy_data_res, energy_data_ind)


##############################################################
# Apply relevance measures to do Feature Selection
# Given it is a supervised dataset, apply supervised
# FS metrics, such as Fisher's Ratio
