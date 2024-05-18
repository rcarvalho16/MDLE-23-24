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

# fratio = Fisher's Ratio
# ig     = Information Gain
# var    = Variance Threshold
fratio_dataset <- read.csv("data\\reduced_fs_fratio.csv", header = TRUE, stringsAsFactors = FALSE)
ig_dataset <- read.csv("data\\reduced_fs_ig.csv", header = TRUE, stringsAsFactors = FALSE)
var_dataset <- read.csv("data\\reduced_fs_var.csv", header = TRUE, stringsAsFactors = FALSE)

df.fratio <- copy_to(sc, fratio_dataset)
df.ig <- copy_to(sc, ig_dataset)
df.var <- copy_to(sc, var_dataset)

################# Model training ###################

# Divide dataset
partitions_fratio <- df.fratio %>% sdf_random_split(training = 2/3, test = 1/3, seed = 1111)
partitions_ig <- df.ig %>% sdf_random_split(training = 2/3, test = 1/3, seed = 1111)
partitions_var <- df.var %>% sdf_random_split(training = 2/3, test = 1/3, seed = 1111)

# Split into training and testing
fratio_training <- partitions_fratio$training
fratio_test <- partitions_fratio$test
ig_training <- partitions_ig$training
ig_test <- partitions_ig$test
var_training <- partitions_var$training
var_test <- partitions_var$test

# Determine number of instances of each class
# Convert Spark DataFrames to R data frames
df.fratio_train_local <- collect(fratio_training)
df.fratio_test_local <- collect(fratio_test)
df.ig_train_local <- collect(ig_training)
df.ig_test_local <- collect(ig_test)
df.var_train_local <- collect(var_training)
df.var_test_local <- collect(var_test)

# Use the table function to determine the number of instances for each class in both datasets
fratio_class_distribution_train <- table(df.fratio_train_local$conditions) %>% as.data.frame()
fratio_class_distribution_test <- table(df.fratio_test_local$conditions) %>% as.data.frame()
ig_class_distribution_train <- table(df.ig_train_local$conditions) %>% as.data.frame()
ig_class_distribution_test <- table(df.ig_test_local$conditions) %>% as.data.frame()
var_class_distribution_train <- table(df.var_train_local$conditions) %>% as.data.frame()
var_class_distribution_test <- table(df.var_test_local$conditions) %>% as.data.frame()

column_names <- c("conditions", "#Instances")
colnames(fratio_class_distribution_train) <- column_names
colnames(fratio_class_distribution_test) <- column_names
colnames(ig_class_distribution_train) <- column_names
colnames(ig_class_distribution_test) <- column_names
colnames(var_class_distribution_train) <- column_names
colnames(var_class_distribution_test) <- column_names

# Display the class distributions
print("Class Distribution in fratio Training Set:")
print(fratio_class_distribution_train)
print("Class Distribution in fratio Testing Set:")
print(fratio_class_distribution_test)

print("Class Distribution in ig Training Set:")
print(ig_class_distribution_train)
print("Class Distribution in ig Testing Set:")
print(ig_class_distribution_test)

print("Class Distribution in var Training Set:")
print(var_class_distribution_train)
print("Class Distribution in var Testing Set:")
print(var_class_distribution_test)

# Model training and evaluation
rf_model_fratio <- fratio_training %>% ml_random_forest(conditions ~ ., type = "regression")
pred_fratio <- ml_predict(rf_model_fratio, fratio_test)
rf_model_ig <- ig_training %>% ml_random_forest(conditions ~ ., type = "regression")
pred_ig <- ml_predict(rf_model_ig, ig_test)
rf_model_var <- var_training %>% ml_random_forest(conditions ~ ., type = "regression")
pred_var <- ml_predict(rf_model_var, var_test)


print("Error Calculations fratio")
ml_regression_evaluations(pred_fratio, label_col = "conditions")
print("Error Calculations ig")
ml_regression_evaluations(pred_ig, label_col = "conditions")
print("Error Calculations var")
ml_regression_evaluations(pred_var, label_col = "conditions")

################# Applying oversampling ##################

# Find the majority class
fratio_class_distribution_train <-
  fratio_class_distribution_train[order(fratio_class_distribution_train$'#Instances', decreasing = TRUE),]
ig_class_distribution_train <-
  ig_class_distribution_train[order(ig_class_distribution_train$'#Instances', decreasing = TRUE),]
var_class_distribution_train <-
  var_class_distribution_train[order(var_class_distribution_train$'#Instances', decreasing = TRUE),]

fratio_majority_class <- fratio_class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric
ig_majority_class <- ig_class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric
var_majority_class <- var_class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric

# Find target sample size for each dataset
fratio_target_size <- fratio_class_distribution_train %>%
  filter(conditions == fratio_majority_class) %>% 
  pull('#Instances')
ig_target_size <- ig_class_distribution_train %>%
  filter(conditions == ig_majority_class) %>% 
  pull('#Instances')
var_target_size <- var_class_distribution_train %>%
  filter(conditions == var_majority_class) %>% 
  pull('#Instances')


# Determine classes that need oversampling for each dataset
fratio_classes_to_oversample <- fratio_class_distribution_train %>%
  filter(conditions != fratio_majority_class) %>%
  pull(conditions) %>% as.numeric
ig_classes_to_oversample <- ig_class_distribution_train %>%
  filter(conditions != ig_majority_class) %>%
  pull(conditions) %>% as.numeric
var_classes_to_oversample <- var_class_distribution_train %>%
  filter(conditions != var_majority_class) %>%
  pull(conditions) %>% as.numeric


# Oversample each class of each dataset
fratio_oversampled_dfs <- lapply(fratio_classes_to_oversample, function(class) {
  sample_class(fratio_training, class, fratio_target_size)
})
ig_oversampled_dfs <- lapply(ig_classes_to_oversample, function(class) {
  sample_class(ig_training, class, ig_target_size)
})
var_oversampled_dfs <- lapply(var_classes_to_oversample, function(class) {
  sample_class(var_training, class, var_target_size)
})



# Combine the oversampled data with the original majority class data
fratio_majority_class_df <- fratio_training %>% filter(conditions == fratio_majority_class)
fratio_oversampled_df <- do.call(sdf_bind_rows, c(list(fratio_majority_class_df), fratio_oversampled_dfs))
ig_majority_class_df <- ig_training %>% filter(conditions == ig_majority_class)
ig_oversampled_df <- do.call(sdf_bind_rows, c(list(ig_majority_class_df), ig_oversampled_dfs))
var_majority_class_df <- var_training %>% filter(conditions == var_majority_class)
var_oversampled_df <- do.call(sdf_bind_rows, c(list(var_majority_class_df), var_oversampled_dfs))

# Check the new class distribution
fratio_new_class_distribution <- fratio_oversampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()
ig_new_class_distribution <- ig_oversampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()
var_new_class_distribution <- var_oversampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()

# Display the class distributions
print("Class Distribution in fratio Training Set:")
print(fratio_new_class_distribution)
# Display the class distributions
print("Class Distribution in ig Training Set:")
print(ig_new_class_distribution)
# Display the class distributions
print("Class Distribution in var Training Set:")
print(var_new_class_distribution)


# Model training and evaluation
rf_model_fratio_oversample <- fratio_oversampled_df %>% ml_random_forest(conditions ~ ., type = "regression")
pred_fratio_oversample <- ml_predict(rf_model_fratio_oversample, fratio_test)
rf_model_ig_oversample <- ig_oversampled_df %>% ml_random_forest(conditions ~ ., type = "regression")
pred_ig_oversample <- ml_predict(rf_model_ig_oversample, ig_test)
rf_model_var_oversample <- var_oversampled_df %>% ml_random_forest(conditions ~ ., type = "regression")
pred_var_oversample <- ml_predict(rf_model_var_oversample, var_test)


print("Error Calculations fratio oversampled")
ml_regression_evaluations(pred_fratio_oversample, label_col = "conditions")
print("Error Calculations ig oversampled")
ml_regression_evaluations(pred_ig_oversample, label_col = "conditions")
print("Error Calculations var oversampled")
ml_regression_evaluations(pred_var_oversample, label_col = "conditions")


################# Applying undersampling ##################

# Find the minority class
fratio_class_distribution_train <-
  fratio_class_distribution_train[order(fratio_class_distribution_train$'#Instances', decreasing = FALSE),]
ig_class_distribution_train <-
  ig_class_distribution_train[order(ig_class_distribution_train$'#Instances', decreasing = FALSE),]
var_class_distribution_train <-
  var_class_distribution_train[order(var_class_distribution_train$'#Instances', decreasing = FALSE),]

fratio_minority_class <- fratio_class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric
ig_minority_class <- ig_class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric
var_minority_class <- var_class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric

# Find target sample size
fratio_target_size <- fratio_class_distribution_train %>%
  filter(conditions == fratio_minority_class) %>% 
  pull('#Instances')
ig_target_size <- ig_class_distribution_train %>%
  filter(conditions == ig_minority_class) %>% 
  pull('#Instances')
var_target_size <- var_class_distribution_train %>%
  filter(conditions == var_minority_class) %>% 
  pull('#Instances')

# Determine classes that need undersampling
fratio_classes_to_undersample <- fratio_class_distribution_train %>%
  filter(conditions != fratio_minority_class) %>%
  pull(conditions) %>% as.numeric
ig_classes_to_undersample <- ig_class_distribution_train %>%
  filter(conditions != ig_minority_class) %>%
  pull(conditions) %>% as.numeric
var_classes_to_undersample <- var_class_distribution_train %>%
  filter(conditions != var_minority_class) %>%
  pull(conditions) %>% as.numeric

# Undersample each class
fratio_undersampled_df <- lapply(fratio_classes_to_undersample, function(class) {
  sample_class(fratio_training, class, fratio_target_size)
})
ig_undersampled_df <- lapply(ig_classes_to_undersample, function(class) {
  sample_class(ig_training, class, ig_target_size)
})
var_undersampled_df <- lapply(var_classes_to_undersample, function(class) {
  sample_class(var_training, class, var_target_size)
})

# Combine the undersampled data with the original minority class data
fratio_minority_class_df <- fratio_training %>% filter(conditions == fratio_minority_class)
fratio_undersampled_df <- do.call(sdf_bind_rows, c(list(fratio_minority_class_df), fratio_undersampled_df))
ig_minority_class_df <- ig_training %>% filter(conditions == ig_minority_class)
ig_undersampled_df <- do.call(sdf_bind_rows, c(list(ig_minority_class_df), ig_undersampled_df))
var_minority_class_df <- fratio_training %>% filter(conditions == var_minority_class)
var_undersampled_df <- do.call(sdf_bind_rows, c(list(var_minority_class_df), var_undersampled_df))

# Check the new class distribution
fratio_new_class_distribution <- fratio_undersampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()
ig_new_class_distribution <- ig_undersampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()
var_new_class_distribution <- var_undersampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()

# Display the class distributions
print("Class Distribution in fratio Training Set:")
print(fratio_new_class_distribution)
print("Class Distribution in ig Training Set:")
print(ig_new_class_distribution)
print("Class Distribution in var Training Set:")
print(var_new_class_distribution)

# Model training and evaluation
rf_model_fratio_undersample <- fratio_undersampled_df %>% ml_random_forest(conditions ~ ., type = "regression")
pred_fratio_undersample <- ml_predict(rf_model_fratio_undersample, fratio_test)
rf_model_ig_undersample <- ig_undersampled_df %>% ml_random_forest(conditions ~ ., type = "regression")
pred_ig_undersample <- ml_predict(rf_model_ig_undersample, ig_test)
rf_model_var_undersample <- var_undersampled_df %>% ml_random_forest(conditions ~ ., type = "regression")
pred_var_undersample <- ml_predict(rf_model_var_undersample, var_test)

print("Error Calculations fratio undersampled")
ml_regression_evaluations(pred_fratio_undersample, label_col = "conditions")
print("Error Calculations ig undersampled")
ml_regression_evaluations(pred_ig_undersample, label_col = "conditions")
print("Error Calculations var undersampled")
ml_regression_evaluations(pred_var_undersample, label_col = "conditions")
