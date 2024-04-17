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
basepath <- "data/Influenza-Outbreak-Dataset"
tr.data <- c(list.files(paste(basepath, "train", sep = "/") , pattern = "train_data_")) #The data to use
labels <- c(list.files(paste(basepath, "train", sep = "/") , pattern = "train_labels_")) #The data to use


fun1 <- function(i) { #read CSV data
  read.csv(paste(basepath,"train",i,sep = "/"), header=FALSE,stringsAsFactors = FALSE)
}

fun2 <- function(i) { #read and transpose CSV data
  read.csv(paste(basepath,"train",i,sep = "/"), header=FALSE,stringsAsFactors = FALSE) %>% t %>% as.data.table
}

df<-do.call(rbind, lapply(tr.data, fun1 )) #bind csv together
df.l<-do.call(rbind, lapply(labels, fun2 )) #bind class together
names(df.l) <-c("CLASS") #rename dependent variable
df.local<- cbind(df.l,df) #bind them together

df <- copy_to(sc, df.local)

################# G2 #######################

# a) Check the schema of the df variable
schema_list <- sdf_schema(df)
types <- sapply(schema_list, function(x) x$type)

# Convert the list to a data frame
schema_df <- data.frame(name = names(schema_list), type = types, stringsAsFactors = FALSE)

# View the data frame
View(schema_df)

# b) Check the content of the SPARK data frame df using the head function
head(df, n = 10)


# Get the actual number of rows and columns in the Spark DataFrame df
actual_rows <- sdf_nrow(df)
actual_cols <- sdf_ncol(df)

# Compare the actual and expected values using stopifnot
# If any expression is not TRUE then it will call stop (exception)
stopifnot(actual_rows == nrow(df.local), actual_cols == ncol(df.local))

################# G3 #######################
#Feature Selection
idx <- c(1,2,5,6,9,10,11,14,16,17,19,21,24,25,26,31,32,33,34,35,41,44,49,50,54)


# Reduce features from df
df.sel <- df %>% select(all_of(idx))

# Overview the resulting dataset
head(df.sel)


################# G4 #######################

# a) Apply the sparklyr sdf random split function to produce two datasets: one for training (2/3) and other
# for testing (1/3). Use the seed value 123, for this and all random functions from this point forward.

# Define the split proportions
train_proportion <- 2 / 3
test_proportion <- 1 - train_proportion

# Split the dataset into training and testing sets
splits <- sdf_random_split(df.sel, training = train_proportion, test = test_proportion, seed = 123)

# Extract the training and testing sets
df.train <- splits[[1]]
df.test <- splits[[2]]

# b) Use the R table function to determine the number of instances for each class in both datasets. Explain
# why this function cannot be used directly on df.train and df.test.

# Convert Spark DataFrames to R data frames
df.train_local <- collect(df.train)
df.test_local <- collect(df.test)

# Use the table function to determine the number of instances for each class in both datasets
class_distribution_train <- table(df.train_local$CLASS) %>% as.data.frame()
class_distribution_test <- table(df.test_local$CLASS) %>% as.data.frame()

column_names <- c("CLASS", "#Instances")
colnames(class_distribution_test) <- column_names
colnames(class_distribution_train) <- column_names

View(class_distribution_test)
View(class_distribution_train)

# Display the class distributions
print("Class Distribution in Training Set:")
print(class_distribution_train)
print("Class Distribution in Testing Set:")
print(class_distribution_test)


# c) Use the ml random forest function to generate a classification model, with the formula: “CLASS ~ .”.

# Train a Random Forest classification model
rf_model <- ml_random_forest(df.train, CLASS ~ ., type = "classification", seed = 123)

# Make predictions on the test dataset
predictions <- mdle.predict(rf_model, df.test)

# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(predictions, "")

################# G5 #######################
# Using imbalanced correcting sampling techniques

# a) What is the number of instances for each class in the training set after the undersampling?

df.pos.train<- df.train %>% filter(CLASS == 1)
df.neg.train<- df.train %>% filter(CLASS == 0)

# Calculate the fraction to be sampled from the majority class to match the minority class
sampling_fraction <- df.pos.train %>% sdf_nrow() / df.neg.train %>% sdf_nrow()

# Given the positive class having way less samples, then we need to undersample the negative class
undersampled_neg <- df.train %>%
  filter(CLASS == 0) %>% sdf_sample(fraction = sampling_fraction, replacement = TRUE, seed = 123)

# Bind both datasets together
df.undersample <- df.pos.train %>% sdf_bind_rows(undersampled_neg)

# Number of instances for each class
df.undersample_local <- collect(df.undersample)
class_distribution_undersample <- table(df.undersample_local$CLASS) %>% as.data.frame()

colnames(class_distribution_undersample) <- column_names
View(class_distribution_undersample)


# b) Repeat points 4.c) and 4.d), and compare the results with the previous models.
undersample_rf_model <- ml_random_forest(df.undersample, CLASS ~ ., type = "classification", seed = 123)

# Make predictions on the test dataset
undersample_predictions <- mdle.predict(undersample_rf_model, df.test)

# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(undersample_predictions, "")



# c) What is the number of instances for each class in the training set after the oversampling?

# Given the positive class having way less samples, then we need to oversample it to match negative class
oversampled_pos <- df.train %>%
  filter(CLASS == 1) %>% sdf_sample(fraction = 1/sampling_fraction, replacement = TRUE, seed = 123)

# Bind both datasets together
df.oversample <- df.neg.train %>% sdf_bind_rows(oversampled_pos)

# Number of instances for each class
df.oversample_local <- collect(df.oversample)
class_distribution_oversample <- table(df.oversample_local$CLASS) %>% as.data.frame()

colnames(class_distribution_oversample) <- column_names
View(class_distribution_oversample)


# d) Repeat points 4.c) and 4.d), and compare the results with the previous models
oversample_rf_model <- ml_random_forest(df.oversample, CLASS ~ ., type = "classification", seed = 123)

# Make predictions on the test dataset
oversample_predictions <- mdle.predict(oversample_rf_model, df.test)

# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(oversample_predictions, "")



#BSMOTE
# e) Apply Borderline-SMOTE Sampling to balance the number of cases of each class
BLSMOTE_train_x <- as.data.frame(collect(df.train %>% select(!CLASS)))
BLSMOTE_train_y <- as.data.frame(collect(df.train %>% select(CLASS)))
BLSMOTE_train_oversampled <- BLSMOTE(X = BLSMOTE_train_x, target = BLSMOTE_train_y, K = 7, C = 4, method = c("type1", "type2"))
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
