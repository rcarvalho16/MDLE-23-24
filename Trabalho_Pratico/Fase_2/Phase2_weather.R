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

fratio_dataset <- read.csv("data\\reduced_fs_fratio.csv", header = TRUE, stringsAsFactors = FALSE)
ig_dataset <- read.csv("data\\reduced_fs_ig.csv", header = TRUE, stringsAsFactors = FALSE)
var_dataset <- read.csv("data\\reduced_fs_var.csv", header = TRUE, stringsAsFactors = FALSE)

df.fratio <- copy_to(sc, fratio_dataset)
df.ig <- copy_to(sc, ig_dataset)
df.var <- copy_to(sc, var_dataset)

################# Model training ###################

# Divide dataset
partitions <- df.fratio %>% sdf_random_split(training = 2/3, test = 1/3, seed = 1111)

# Split into training and testing
fratio_training <- partitions$training
fratio_test <- partitions$test

# Determine number of instances of each class
# Convert Spark DataFrames to R data frames
df.train_local <- collect(fratio_training)
df.test_local <- collect(fratio_test)

# Use the table function to determine the number of instances for each class in both datasets
class_distribution_train <- table(df.train_local$conditions) %>% as.data.frame()
class_distribution_test <- table(df.test_local$conditions) %>% as.data.frame()

column_names <- c("conditions", "#Instances")
colnames(class_distribution_test) <- column_names
colnames(class_distribution_train) <- column_names

# Display the class distributions
print("Class Distribution in Training Set:")
print(class_distribution_train)
print("Class Distribution in Testing Set:")
print(class_distribution_test)

# Model training and evaluation
rf_model_fratio <- fratio_training %>% ml_random_forest(conditions ~ ., type = "regression")
pred_fratio <- ml_predict(rf_model_fratio, fratio_test)

print("Error Calculations")
ml_regression_evaluations(pred_fratio, label_col = "conditions")

################# Applying oversampling ##################

# Find the majority class
class_distribution_train <- 
  class_distribution_train[order(class_distribution_train$'#Instances', decreasing = TRUE),]

majority_class <- class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric

# Find target sample size
target_size <- class_distribution_train %>%
  filter(conditions == majority_class) %>% 
  pull('#Instances')

# Determine classes that need oversampling
classes_to_oversample <- class_distribution_train %>%
  filter(conditions != majority_class) %>%
  pull(conditions) %>% as.numeric


# Oversample each class
oversampled_dfs <- lapply(classes_to_oversample, function(class) {
  sample_class(fratio_training, class, target_size)
})


# Combine the oversampled data with the original majority class data
majority_class_df <- fratio_training %>% filter(conditions == majority_class)
oversampled_df <- do.call(sdf_bind_rows, c(list(majority_class_df), oversampled_dfs))

# Check the new class distribution
new_class_distribution <- oversampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()

# Display the class distributions
print("Class Distribution in Training Set:")
print(new_class_distribution)


# Model training and evaluation
rf_model_fratio_oversample <- oversampled_df %>% ml_random_forest(conditions ~ ., type = "regression")
pred_fratio_oversample <- ml_predict(rf_model_fratio_oversample, fratio_test)

print("Error Calculations")
ml_regression_evaluations(pred_fratio_oversample, label_col = "conditions")


################# Applying undersampling ##################

# Find the minority class
class_distribution_train <- 
  class_distribution_train[order(class_distribution_train$'#Instances', decreasing = FALSE),]

minority_class <- class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric

# Find target sample size
target_size <- class_distribution_train %>%
  filter(conditions == minority_class) %>% 
  pull('#Instances')

# Determine classes that need oversampling
classes_to_undersample <- class_distribution_train %>%
  filter(conditions != minority_class) %>%
  pull(conditions) %>% as.numeric

# Undersample each class
undersampled_df <- lapply(classes_to_undersample, function(class) {
  sample_class(fratio_training, class, target_size)
})

# Combine the undersampled data with the original minority class data
minority_class_df <- fratio_training %>% filter(conditions == minority_class)
undersampled_df <- do.call(sdf_bind_rows, c(list(minority_class_df), undersampled_df))

# Check the new class distribution
new_class_distribution <- undersampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()

# Display the class distributions
print("Class Distribution in Training Set:")
print(new_class_distribution)

# Model training and evaluation
rf_model_fratio_undersample <- undersampled_df %>% ml_random_forest(conditions ~ ., type = "regression")
pred_fratio_undersample <- ml_predict(rf_model_fratio_undersample, fratio_test)

print("Error Calculations")
ml_regression_evaluations(pred_fratio_undersample, label_col = "conditions")
