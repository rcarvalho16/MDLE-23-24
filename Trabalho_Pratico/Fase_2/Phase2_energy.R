################# Preparation ################
#install.packages(c("dplyr", "sparklyr", "smotefamily", "data.table", "caret", "pROC"))
library(pROC)
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

df.freduction_energy <- copy_to(sc, freduction_energy)

################# Model training ###################
# Define the split proportions
train_proportion <- 2 / 3
test_proportion <- 1 - train_proportion

# Split the dataset into training and testing sets
splits <- sdf_random_split(df.freduction_energy, training = train_proportion, test = test_proportion, seed = 333)

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
rf_model <- ml_random_forest(df.train, labels ~ ., type = "classification", seed = 333)
lr_model <- ml_logistic_regression(df.train, labels ~ .)
dt_model <- ml_decision_tree_classifier(df.train, labels ~ ., seed = 333)

# Make predictions on the test dataset
rf_predictions <- mdle.predict(rf_model, df.test)
lr_predictions <- mdle.predict(lr_model, df.test)
dt_predictions <- mdle.predict(dt_model, df.test)


# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(rf_predictions, "")
mdle.printConfusionMatrix(lr_predictions, "")
mdle.printConfusionMatrix(dt_predictions, "")



rf_predictions <- rf_predictions %>% select('labels', 'prediction')  %>% collect
roc_value <- roc(df.test_local$labels, rf_predictions$prediction)
auc_value <- auc(roc_value)
print("Area under ROC:")
print(auc_value)

lr_predictions <- lr_predictions %>% select('labels', 'prediction')  %>% collect
roc_value <- roc(df.test_local$labels, lr_predictions$prediction)
auc_value <- auc(roc_value)
print("Area under ROC:")
print(auc_value)

dt_predictions <- dt_predictions %>% select('labels', 'prediction')  %>% collect
roc_value <- roc(df.test_local$labels, dt_predictions$prediction)
auc_value <- auc(roc_value)
print("Area under ROC:")
print(auc_value)



################# Applying undersampling ##################


df.pos.train<- df.train %>% filter(labels == 1) # Industrial
df.neg.train<- df.train %>% filter(labels == 2) # Residential

# Calculate the fraction to be sampled from the majority class to match the minority class
sampling_fraction <- df.pos.train %>% sdf_nrow() / df.neg.train %>% sdf_nrow()

# Given the positive class having way less samples, then we need to undersample the negative class
undersampled_neg <- df.train %>%
  filter(labels == 2) %>% sdf_sample(fraction = sampling_fraction, replacement = TRUE, seed = 333)

# Bind both datasets together
df.undersample <- df.pos.train %>% sdf_bind_rows(undersampled_neg)

# Number of instances for each class
df.undersample_local <- collect(df.undersample)
class_distribution_undersample <- table(df.undersample_local$labels) %>% as.data.frame()
colnames(class_distribution_undersample) <- column_names
View(class_distribution_undersample)

# Train the model again
undersample_rf_model <- ml_random_forest(df.undersample, labels ~ ., type = "classification", seed = 333)
undersample_lr_model <- ml_logistic_regression(df.undersample, labels ~ .)
undersample_dt_model <- ml_decision_tree_classifier(df.undersample, labels ~ ., seed = 333)

# Make predictions on the test dataset
undersample_rf_predictions <- mdle.predict(undersample_rf_model, df.test)
undersample_lr_predictions <- mdle.predict(undersample_lr_model, df.test)
undersample_dt_predictions <- mdle.predict(undersample_dt_model, df.test)


# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(undersample_rf_predictions, "")
mdle.printConfusionMatrix(undersample_lr_predictions, "")
mdle.printConfusionMatrix(undersample_dt_predictions, "")

undersample_rf_predictions <- undersample_rf_predictions %>% select('labels', 'prediction')  %>% collect
roc_value <- roc(df.test_local$labels, undersample_rf_predictions$prediction)
auc_value <- auc(roc_value)
print("Area under ROC:")
print(auc_value)

undersample_lr_predictions <- undersample_lr_predictions %>% select('labels', 'prediction')  %>% collect
roc_value <- roc(df.test_local$labels, undersample_lr_predictions$prediction)
auc_value <- auc(roc_value)
print("Area under ROC:")
print(auc_value)

undersample_dt_predictions <- undersample_dt_predictions %>% select('labels', 'prediction')  %>% collect
roc_value <- roc(df.test_local$labels, undersample_dt_predictions$prediction)
auc_value <- auc(roc_value)
print("Area under ROC:")
print(auc_value)

################# Applying oversampling ##################

# Given the positive class having way less samples, then we need to oversample it to match negative class
oversampled_pos <- df.train %>%
  filter(labels == 1) %>% sdf_sample(fraction = 1/sampling_fraction, replacement = TRUE, seed = 333)

# Bind both datasets together
df.oversample <- df.neg.train %>% sdf_bind_rows(oversampled_pos)

# Number of instances for each class
df.oversample_local <- collect(df.oversample)
class_distribution_oversample <- table(df.oversample_local$labels) %>% as.data.frame()
colnames(class_distribution_oversample) <- column_names
View(class_distribution_oversample)


# Train the model again
oversample_rf_model <- ml_random_forest(df.oversample, labels ~ ., type = "classification", seed = 333)
oversample_lr_model <- ml_logistic_regression(df.oversample, labels ~ .)
oversample_dt_model <- ml_decision_tree_classifier(df.oversample, labels ~ ., seed = 333)

# Make predictions on the test dataset
oversample_rf_predictions <- mdle.predict(oversample_rf_model, df.test)
oversample_lr_predictions <- mdle.predict(oversample_lr_model, df.test)
oversample_dt_predictions <- mdle.predict(oversample_dt_model, df.test)

# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(oversample_rf_predictions, "")
mdle.printConfusionMatrix(oversample_lr_predictions, "")
mdle.printConfusionMatrix(oversample_dt_predictions, "")

oversample_rf_predictions <- oversample_rf_predictions %>% select('labels', 'prediction')  %>% collect
roc_value <- roc(df.test_local$labels, oversample_rf_predictions$prediction)
auc_value <- auc(roc_value)
print("Area under ROC:")
print(auc_value)

oversample_lr_predictions <- oversample_lr_predictions %>% select('labels', 'prediction')  %>% collect
roc_value <- roc(df.test_local$labels, oversample_lr_predictions$prediction)
auc_value <- auc(roc_value)
print("Area under ROC:")
print(auc_value)

oversample_dt_predictions <- oversample_dt_predictions %>% select('labels', 'prediction')  %>% collect
roc_value <- roc(df.test_local$labels, oversample_dt_predictions$prediction)
auc_value <- auc(roc_value)
print("Area under ROC:")
print(auc_value)

################# Spark cleanup ################
spark_disconnect(sc)
