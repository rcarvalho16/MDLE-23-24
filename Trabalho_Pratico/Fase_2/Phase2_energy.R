################# Preparation ################
#install.packages(c("dplyr", "sparklyr", "smotefamily", "data.table", "caret"))
library(dplyr) #data manipulation
library(sparklyr) #spark 
library(smotefamily) #For SMOTE sampling
library(data.table) #To be used when possible, as a more performant data.frame

if(!exists("printConfusionMatrix", mode="function")) 
  source("helperfunctions.R")

################# Spark setup ################

spark_disconnect_all() #just preventive code
#options(sparklyr.log.console = TRUE)
sc <- spark_connect("local", version = '3.4.2', hadoop_version = '3', config = list())

################# Load data ################
basepath <- "data"
tr.data <- c(list.files(basepath , pattern = ".csv")) #The data to use

freduction_energy <- read.csv("data\\reduced_fr_energy.csv", header = TRUE, stringsAsFactors = FALSE)
fselection_energy <- read.csv("data\\reduced_fs_energy.csv", header = TRUE, stringsAsFactors = FALSE)

# Since BLSMOTE uses only numeric values, we must convert all discretized ranges
# To a numeric equivalent

freduction_energy[1:5] <- lapply(freduction_energy[1:5], as.factor) %>% lapply(as.numeric)

fselection_energy[2] <- lapply(freduction_energy[2], as.factor) %>% lapply(as.numeric)
colnames(fselection_energy)[3] <- "labels"

df.freduction_energy <- copy_to(sc, freduction_energy)
df.fselection_energy <- copy_to(sc, fselection_energy)

################# Model training ###################
# Define the split proportions
train_proportion <- 2 / 3
test_proportion <- 1 - train_proportion

# Split the dataset into training and testing sets
splits <- sdf_random_split(df.freduction_energy, training = train_proportion, test = test_proportion, seed = 123)

# Extract the training and testing sets
df.train <- splits[[1]]
df.test <- splits[[2]]

# Determine number of instances of each class

# Convert Spark DataFrames to R data frames
df.train_local <- collect(df.train)
df.test_local <- collect(df.test)

# Use the table function to determine the number of instances for each class in both datasets
class_distribution_train <- table(df.train_local$labels) %>% as.data.frame()
class_distribution_test <- table(df.test_local$labels) %>% as.data.frame()

column_names <- c("CLASS", "#Instances")
colnames(class_distribution_test) <- column_names
colnames(class_distribution_train) <- column_names

# Display the class distributions
print("Class Distribution in Training Set:")
print(class_distribution_train)
print("Class Distribution in Testing Set:")
print(class_distribution_test)

# Class 1: Industrial county
# Class 2: Residential county

# Train a Random Forest classification model
rf_model <- ml_random_forest(df.train, labels ~ ., type = "classification", seed = 123)

# Make predictions on the test dataset
predictions <- mdle.predict(rf_model, df.test)

# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(predictions, "")

################# Applying undersampling ##################


df.pos.train<- df.train %>% filter(labels == 1) # Industrial
df.neg.train<- df.train %>% filter(labels == 2) # Residential

# Calculate the fraction to be sampled from the majority class to match the minority class
sampling_fraction <- df.pos.train %>% sdf_nrow() / df.neg.train %>% sdf_nrow()

# Given the positive class having way less samples, then we need to undersample the negative class
undersampled_neg <- df.train %>%
  filter(labels == 2) %>% sdf_sample(fraction = sampling_fraction, replacement = TRUE, seed = 123)

# Bind both datasets together
df.undersample <- df.pos.train %>% sdf_bind_rows(undersampled_neg)

# Number of instances for each class
df.undersample_local <- collect(df.undersample)
class_distribution_undersample <- table(df.undersample_local$labels) %>% as.data.frame()
colnames(class_distribution_undersample) <- column_names
View(class_distribution_undersample)

# Train the model again
undersample_rf_model <- ml_random_forest(df.undersample, labels ~ ., type = "classification", seed = 123)

# Make predictions on the test dataset
undersample_predictions <- mdle.predict(undersample_rf_model, df.test)

# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(undersample_predictions, "")

################# Applying oversampling ##################

# Given the positive class having way less samples, then we need to oversample it to match negative class
oversampled_pos <- df.train %>%
  filter(labels == 1) %>% sdf_sample(fraction = 1/sampling_fraction, replacement = TRUE, seed = 123)

# Bind both datasets together
df.oversample <- df.neg.train %>% sdf_bind_rows(oversampled_pos)

# Number of instances for each class
df.oversample_local <- collect(df.oversample)
class_distribution_oversample <- table(df.oversample_local$labels) %>% as.data.frame()
colnames(class_distribution_oversample) <- column_names
View(class_distribution_oversample)


# Train the model again
oversample_rf_model <- ml_random_forest(df.oversample, labels ~ ., type = "classification", seed = 123)

# Make predictions on the test dataset
oversample_predictions <- mdle.predict(oversample_rf_model, df.test)

# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(oversample_predictions, "")

################# Applying BL-SMOTE #################

# To apply BLSMOTE, all values must be numeric and therefore we need to convert
# the discretized ranges to numeric values.
# NÃO CONSIGO APLICAR NÃO COMPREENDO PORQUÊ (JÁ OLHEI PARA O SOURCE CODE DA FUNÇÃO
# E COMPREENDO PORQUE NÃO CORRE MAS NÃO COMPREENDO PORQUE É QUE DÁ AQUELES RESULTS)


BLSMOTE_train_x <- as.data.frame(collect(df.train %>% select(!labels)))
BLSMOTE_train_y <- as.data.frame(collect(df.train %>% select(labels)))

BLSMOTE_train_oversampled <- BLSMOTE(X = BLSMOTE_train_x, target = BLSMOTE_train_y, K = 5, C = 5, method = c("type1","type2"))

BLSMOTE_train_oversampled <- as.data.frame(BLSMOTE_train_oversampled$data)

BLSMOTE_train_oversampled <- copy_to(sc, BLSMOTE_train_oversampled)

# f) Repeat points 4.c) and 4.d), and compare the results with the previous models
oversample_blsmote_rf_model <- ml_random_forest(BLSMOTE_train_oversampled, class ~ ., type = "classification", seed = 123)

# Make predictions on the test dataset
oversample_blsmote_predictions <- mdle.predict(oversample_blsmote_rf_model, df.test)

# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(oversample_blsmote_predictions, "")

################# Spark cleanup ################
spark_disconnect(sc)
