library(dplyr)
library(ggplot2)
library(lubridate)
library(cowplot)
library(ggcorrplot)
library(factoextra)

# AUX FUNCTIONS
######################
readEnergyFun <- function(i) { #read CSV data
  read.csv(paste(data_path,i,sep = "/"), header=TRUE,stringsAsFactors = FALSE, sep = ";")
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
  dataset <- dataset[, -c(1,2,3)]
  # Reorder columns
  dataset <- dataset[, c(6,5,4,7,3,1,2)]
  
  return(dataset)
}

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
energy_data_res <- convertTimestamps(energy_data_res)



# Compute average consumption on all days of the week
average_consumption_weekday <- energy_data_res %>%
  group_by(Zip.Code, Day_of_Week) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Compare average consumption per hour
average_consumption_hour <- energy_data_res %>%
  group_by(Zip.Code, Hours) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Normalize hourly consumption values
normalized_average_consumption_hour <- average_consumption_hour %>%
  group_by(Zip.Code) %>%
  mutate(Average_Consumption = Average_Consumption / max(Average_Consumption))

# Convert Zip Codes to integer for further analysis
energy_data_res$Zip.Code <- sapply(energy_data_res$Zip.Code, as.integer) 

# Examples of non residential Zip Codes:
# Industrial sites, these will be classified as Non-Residential
ind_zip_codes <- c(7520, 2350, 4970, 2840, 3800, 6300, 2900, 3850, 3860, 2430, 3530, 7600,
                   7780, 7220, 4760)
energy_data_ind <- energy_data[energy_data$Zip.Code %in% ind_zip_codes, ]
energy_data_ind <- convertTimestamps(energy_data_ind)


# Compute average consumption
average_consumption_weekday_ind <- energy_data_ind %>%
  group_by(Zip.Code, Day_of_Week) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Compare average consumption per hour
average_consumption_hour_ind <- energy_data_ind %>%
  group_by(Zip.Code, Hours) %>%
  summarise(Average_Consumption = mean(Active.Energy..kWh.))

# Normalize hourly consumption values
normalized_average_consumption_hour_ind <- average_consumption_hour_ind %>%
  group_by(Zip.Code) %>%
  mutate(Average_Consumption = Average_Consumption / max(Average_Consumption))

# Convert Zip Codes to integer for further analysis
energy_data_ind$Zip.Code <- sapply(energy_data_ind$Zip.Code, as.integer)

#################################################################################
# Compare residential consumption and industrial consumption


# Line chart for hourly consumption in residential zones
linha_res = ggplot(normalized_average_consumption_hour, aes(x = Hours, y = Average_Consumption, color = factor(Zip.Code))) +
  geom_line(aes(group = Zip.Code)) +
  ylim(0,1) +
  labs(title = "Average Consumption by Hour and Zip Code [Residential]",
       x = "Hour of the Day",
       y = "Average Consumption (kWh)",
       color = "Zip Code") +
  theme_minimal()

# Line chart for hourly consumption in industrial zones
linha_ind = ggplot(normalized_average_consumption_hour_ind, aes(x = Hours, y = Average_Consumption, color = factor(Zip.Code))) +
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

# Given the categorical data, and thinking about possible learning models we can
# apply. Convert dataset using one-hot encoding for day of the week 
energy_data_labeled$Zone <- factor(energy_data_labeled$Zone, labels = c(1,2))
energy_data_labeled$Day_of_Week <- factor(energy_data_labeled$Day_of_Week, labels = c(1,2,3,4,5,6,7))

##############################################################
# FEATURE REDUCTION
# PCA Analysis
# Note: Apply PCA only to features (excluding class label)
# PCA applies a linear transformation, therefore
# We reattach the class labels again after PCA FR

# Convert all factors to integers
energy_data_labeled[] <- lapply(energy_data_labeled, function(x) {
  if(is.factor(x)) {
    as.integer(x)
  } else {
    x
  }
})

# Observe correlation plot
ggcorrplot(cor(energy_data_labeled))

# Store labels 
labels <- energy_data_labeled$Zone

# Prepare matrix for PCA (remove label in last column)
pca_energy <- energy_data_labeled[-ncol(energy_data_labeled)]

# Apply PCA to the matrices
unscaled_energy_pca <- prcomp(pca_energy, scale. = FALSE)
scaled_energy_pca <- prcomp(pca_energy, scale. = TRUE)

summary_unscaled_energy_pca <- summary(unscaled_energy_pca)
summary_scaled_energy_pca <- summary(scaled_energy_pca)

# Quick summary of PCA results:
summary_unscaled_energy_pca
summary_scaled_energy_pca

# For memory purposes
rm(summary_unscaled_energy_pca)
rm(summary_scaled_energy_pca)

# Plot PCA results
fviz_eig(unscaled_energy_pca, addlabels = TRUE)
fviz_eig(scaled_energy_pca, addlabels = TRUE)

# The eigenvalues display the variance of the original data preserved
eig_values_unscaled = get_eig(unscaled_energy_pca)
eig_values_scaled = get_eig(scaled_energy_pca)

# Preserve only principal components whose eigen values compute 95% of the variance
adequate_dimensions_unscaled <- min(which(eig_values_unscaled$cumulative.variance.percent > 95))
adequate_dimensions_scaled <- min(which(eig_values_scaled$cumulative.variance.percent > 95))

# From the previous result we can tell unscaled will produce poor results, given the 
# adequate reduced dimensionality to be 1 so we can discard that option.

# Keep only the PCs that compute minimum adequate dimensions
pcs_scaled <- scaled_energy_pca$rotation[, 1:adequate_dimensions_scaled]

# Compute the newly reduced dataset by applying the product of the original dataset
# With the pca components
rd_pca_scaled <- as.matrix(pca_energy) %*% as.matrix(pcs_scaled)

# Reattach the class labels to the newly reduced matrix
rd_pca_scaled <- as.matrix(rd_pca_scaled)
rd_pca_scaled <- cbind(rd_pca_scaled, labels)

# Observe correlation plot
ggcorrplot(cor(rd_pca_scaled))
