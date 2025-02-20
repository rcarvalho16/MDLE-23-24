libs <- c("dplyr","ggplot2","lubridate","cowplot","ggcorrplot","factoextra","arulesCBA","FSelectorRcpp", "caret", "e1071")

# Install libraries, uncomment only on the 1st run
# install.packages(libs)

# Load libraries
sapply(libs, library, character.only = TRUE)
rm(libs)


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



# Check empty rows in weather dataset
# Commented because it will return 0 missing values, and takes a while to run
# apply(energy_data, MARGIN = 2, function(col) sum(is.na(col)))

# Check Standard deviation and mean value of numeric features
calculate_summary(energy_data)

# Perform analysis on Residential vs Non-Residential ZIP codes
# This classification was done by hand. Prone to human-error
# Ref: https://www.pordata.pt/db/municipios/ambiente+de+consulta/tabela

# Examples of residential Zip Codes:
# These will be classified as Residential
res_zip_codes <- c(1000, 1600, 1700, 2120, 2230, 2675, 2735, 2745, 3250, 3600, 3610, 3630, 4660, 4910, 6060,
                   7240, 7350, 7630, 7700, 8400, 8670, 8700)
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
# We reattach the class labels and zip codes again after PCA FR

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
labels <- as.numeric(energy_data_labeled$Zone)
zip_codes <- energy_data_labeled$Zip.Code

# Prepare matrix for PCA (remove label and zip code in last columns)
remove <- c("Zone","Zip.Code")

# Prepared matrix for PCA
pca_energy <- energy_data_labeled[,!names(energy_data_labeled) %in% remove]

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

# Observe correlation plot
ggcorrplot(cor(rd_pca_scaled))

rd_pca_scaled <- as.data.frame(rd_pca_scaled)
# Compute discretization on features
for(i in 1:ncol(rd_pca_scaled)){
  rd_pca_scaled[,i] <- equalFrequencyBinning(rd_pca_scaled[,i])
}
rm(i)

# Reattach zip codes and labels
rd_pca_scaled <- data.frame(cbind(rd_pca_scaled, zip_codes,labels))

# Check discretized pca reduced dataset
View(rd_pca_scaled)



##############################################################
# FEATURE SELECTION
# Fisher's Ratio

# Removing zip codes as they are a numeric but symbolic value
remove <- c("Zip.Code")
fr_no_zip <- energy_data_labeled[,!names(energy_data_labeled) %in% remove]

# Prepare fisher's ratio matrix by removing Zip codes (Unique variable)
# Zips are removed because they are a numeric, but symbolic value
fr_energy_unscaled <- fr_no_zip
fr_energy_scaled <- as.data.frame(round(scale(fr_no_zip), 3))

# Calculate class means and class vars to perform Fisher's Ratio (scaled and unscaled)
class_means_unscaled <- getClassMeans(fr_energy_unscaled, "Zone")
class_vars_unscaled <- getClassVars(fr_energy_unscaled, "Zone")

class_means_scaled <- getClassMeans(fr_energy_scaled, "Zone")
class_vars_scaled <- getClassVars(fr_energy_scaled, "Zone")

# Compute numerator and denominator for calculation

fr_numerator_unscaled = apply(class_means_unscaled, MARGIN = 1, function(x) (x[1] - sum(x[-1]))^2)
fr_denominator_unscaled =  apply(class_vars_unscaled, MARGIN = 1, sum)

fr_numerator_scaled = apply(class_means_scaled, MARGIN = 1, function(x) (x[1] - sum(x[-1]))^2)
fr_denominator_scaled =  apply(class_vars_scaled, MARGIN = 1, sum)

# Compute final fisher's ratio
fr_unscaled = fr_numerator_unscaled / fr_denominator_unscaled
fr_scaled = fr_numerator_scaled / fr_denominator_scaled



#################### Information Gain ##############################
ig_energy = information_gain(formula = Zone ~ ., data = energy_data_labeled, type = "infogain")

# Make ig_energy same format as fr_scaled and fr_unscaled
names <- ig_energy$attributes[-length(ig_energy$attributes)]
ig_energy <- as.data.frame(ig_energy[,-1])
# Remove Zip.Code as it is a unique value and does not make sense in IG
ig_energy <- ig_energy[-nrow(ig_energy),]
ig_energy <- as.data.frame(ig_energy)
rownames(ig_energy) <- names
colnames(ig_energy) <- "Information Gain"


# Bind both metrics together for comparison
View(cbind(fr_scaled, fr_unscaled, ig_energy))

###################### FS Reduction #############################
# Given both FS results, we can pretty much reduce our dataset to be the Zip.Codes, Class label, and Active Consumption

fs_reduced_energy <- energy_data_labeled[, c("Zip.Code", "Active.Energy..kWh.", "Zone")]

# Apply discretization by Sturges Law
for(i in 1:ncol(fs_reduced_energy)){
  fs_reduced_energy[,i] <- equalFrequencyBinning(fs_reduced_energy[,i])
}

View(fs_reduced_energy)


# Randomly select 500 indices
set.seed(123)
random_indices <- sample(1:nrow(fs_reduced_energy), 500)

# Subset the dataframe using the randomly selected indices
subset_df <- fs_reduced_energy[random_indices, ]

# Plot the discretized consumption per zone
ggplot(
  data = subset_df,
  aes(x = Zone, y = `Active.Energy..kWh.`, label = ifelse(Zone == 1, "Industrial", "Residential"), color = as.factor(Zone))) +
  geom_point(size = 3) +
  labs(x = "Zone", y = "Active Energy (kWh)", color = "Zone", title = "Discretized Consumption Per Zone") +
  scale_color_manual(labels = c("Industrial", "Residencial"), values = c("blue", "red")) +
  scale_x_continuous(breaks = seq(0, 3, 1)) +  # Adjust Y axis to start from 0 with unitary steps
  theme_minimal() +
  theme(axis.title = element_text(size = 12),  # Adjust axis label size
        axis.text = element_text(size = 10))  # Adjust axis tick label size


# Saving files to data
#write.csv(fs_reduced_energy, "Path\\reduced_fs_energy.csv", row.names = FALSE)
#write.csv(rd_pca_scaled, "Path\\reduced_fr_energy.csv", row.names = FALSE)


############ TODO #################

# Compare consumption per hour of all days of a month
# And try to create a new dataset labeling if there or there wasn't an event at 
# that day and time
# Example, Football game day, Zip Code = 1500



