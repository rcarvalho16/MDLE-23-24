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

# fr = Fisher's Ratio
fr_dataset <- read.csv("data\\reduced_fs_fratio.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(fr_dataset)[3] <- "Active_Energy_kWh"

df.fratio <- copy_to(sc, fr_dataset, overwrite = TRUE)

################# Model training ###################

# Divide dataset
partitions_fratio <- df.fratio %>% sdf_random_split(training = 2/3, test = 1/3, seed = 1111)

# Split into training and testing
fratio_training <- partitions_fratio$training
fratio_test <- partitions_fratio$test

# Determine number of instances of each class
# Convert Spark DataFrames to R data frames
df.fratio_train_local <- collect(fratio_training)
df.fratio_test_local <- collect(fratio_test)

# Use the table function to determine the number of instances for each class in both datasets
fratio_class_distribution_train <- table(df.fratio_train_local$conditions) %>% as.data.frame()
fratio_class_distribution_test <- table(df.fratio_test_local$conditions) %>% as.data.frame()

column_names <- c("conditions", "#Instances")
colnames(fratio_class_distribution_train) <- column_names
colnames(fratio_class_distribution_test) <- column_names

print("Class Distribution in ig Training Set:")
print(fratio_class_distribution_train)
print("Class Distribution in ig Testing Set:")
print(fratio_class_distribution_test)

# Model training and evaluation
lr_model_fratio <- fratio_training %>% ml_linear_regression(formula = "Active_Energy_kWh ~ .")
dt_model_fratio <- fratio_training %>% ml_decision_tree_regressor(formula = "Active_Energy_kWh ~ .")
gbt_model_fratio <- fratio_training %>% ml_gradient_boosted_trees(formula = "Active_Energy_kWh ~ .")

lr_pred_fratio <- ml_predict(lr_model_fratio, fratio_test)
dt_pred_fratio <- ml_predict(dt_model_fratio, fratio_test)
gbt_pred_fratio <- ml_predict(gbt_model_fratio, fratio_test)

print("Error Calculations ig")
ml_regression_evaluations(lr_pred_fratio, label_col = "Active_Energy_kWh")
ml_regression_evaluations(dt_pred_fratio, label_col = "Active_Energy_kWh")
ml_regression_evaluations(gbt_pred_fratio, label_col = "Active_Energy_kWh")

################# Applying oversampling ##################

# Find the majority class
fratio_class_distribution_train <-
  fratio_class_distribution_train[order(fratio_class_distribution_train$'#Instances', decreasing = TRUE),]

fratio_majority_class <- fratio_class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric

# Find target sample size for each dataset
fratio_target_size <- fratio_class_distribution_train %>%
  filter(conditions == fratio_majority_class) %>% 
  pull('#Instances')

# Determine classes that need oversampling
fratio_classes_to_oversample <- fratio_class_distribution_train %>%
  filter(conditions != fratio_majority_class) %>%
  pull(conditions) %>% as.numeric


# Oversample each class
fratio_oversampled_dfs <- lapply(fratio_classes_to_oversample, function(class) {
  sample_class(fratio_training, class, fratio_target_size)
})


# Combine the oversampled data with the original majority class data
fratio_majority_class_df <- fratio_training %>% filter(conditions == fratio_majority_class)
fratio_oversampled_df <- do.call(sdf_bind_rows, c(list(fratio_majority_class_df), fratio_oversampled_dfs))

# Check the new class distribution
fratio_new_class_distribution <- fratio_oversampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()

# Display the class distributions
print("Class Distribution in ig Training Set:")
print(fratio_new_class_distribution)

# Model training and evaluation
lr_model_fratio <- fratio_oversampled_df %>% ml_linear_regression(formula = "Active_Energy_kWh ~ .")
dt_model_fratio <- fratio_oversampled_df %>% ml_decision_tree_regressor(formula = "Active_Energy_kWh ~ .")
gbt_model_fratio <- fratio_oversampled_df %>% ml_gradient_boosted_trees(formula = "Active_Energy_kWh ~ .")

lr_pred_fratio <- ml_predict(lr_model_fratio, fratio_test)
dt_pred_fratio <- ml_predict(dt_model_fratio, fratio_test)
gbt_pred_fratio <- ml_predict(gbt_model_fratio, fratio_test)

print("Error Calculations ig")
ml_regression_evaluations(lr_pred_fratio, label_col = "Active_Energy_kWh")
ml_regression_evaluations(dt_pred_fratio, label_col = "Active_Energy_kWh")
ml_regression_evaluations(gbt_pred_fratio, label_col = "Active_Energy_kWh")

################# Applying undersampling ##################

# Find the minority class
fratio_class_distribution_train <-
  fratio_class_distribution_train[order(fratio_class_distribution_train$'#Instances', decreasing = FALSE),]

fratio_minority_class <- fratio_class_distribution_train %>% slice(1) %>% pull(conditions) %>% as.numeric

# Find target sample size
fratio_target_size <- fratio_class_distribution_train %>%
  filter(conditions == fratio_minority_class) %>% 
  pull('#Instances')

# Determine classes that need undersampling
fratio_classes_to_undersample <- fratio_class_distribution_train %>%
  filter(conditions != fratio_minority_class) %>%
  pull(conditions) %>% as.numeric

# Undersample each class
fratio_undersampled_df <- lapply(fratio_classes_to_undersample, function(class) {
  sample_class(fratio_training, class, fratio_target_size)
})

# Combine the undersampled data with the original minority class data
fratio_minority_class_df <- fratio_training %>% filter(conditions == fratio_minority_class)
fratio_undersampled_df <- do.call(sdf_bind_rows, c(list(fratio_minority_class_df), fratio_undersampled_df))

# Check the new class distribution
fratio_new_class_distribution <- fratio_undersampled_df %>%
  group_by(conditions) %>%
  summarise(count = n()) %>%
  collect()

# Display the class distributions
print("Class Distribution in ig Training Set:")
print(fratio_new_class_distribution)

# Model training and evaluation
lr_model_fratio <- fratio_undersampled_df %>% ml_linear_regression(formula = "Active_Energy_kWh ~ .")
dt_model_fratio <- fratio_undersampled_df %>% ml_decision_tree_regressor(formula = "Active_Energy_kWh ~ .")
gbt_model_fratio <- fratio_undersampled_df %>% ml_gradient_boosted_trees(formula = "Active_Energy_kWh ~ .")

lr_pred_fratio <- ml_predict(lr_model_fratio, fratio_test)
dt_pred_fratio <- ml_predict(dt_model_fratio, fratio_test)
gbt_pred_fratio <- ml_predict(gbt_model_fratio, fratio_test)

print("Error Calculations ig")
ml_regression_evaluations(lr_pred_fratio, label_col = "Active_Energy_kWh")
ml_regression_evaluations(dt_pred_fratio, label_col = "Active_Energy_kWh")
ml_regression_evaluations(gbt_pred_fratio, label_col = "Active_Energy_kWh")


