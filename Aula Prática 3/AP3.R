################# Preparation ################
install.packages(c("dplyr", "sparklyr", "smotefamily", "data.table", "caret"))
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

# a) Check the schema of the df variable
sdf_schema(df)


################# G2 #######################
# b) Check the content of the SPARK data frame df using the head function
head(df)

# Get a glimpse of the first and last 10 rows of the dataset
head(df, n = 10)

#Glimpse of the data set
# Function stopifnot() prints NULL if all statements are true
# Sparklyr's equivalent of nrow is sdf_nrow()
print(stopifnot(sdf_nrow(df) != 2190))


# Get the actual number of rows and columns in the Spark DataFrame df
actual_rows <- nrow(df)
actual_cols <- ncol(df)
# Compare the actual and expected values using stopifnot
stopifnot(actual_rows == expected_rows,
          actual_cols == expected_cols)

################# G3 #######################
#Feature Selection
idx <- c(1,2,5,6,9,10,11,14,16,17,19,21,24,25,26,31,32,33,34,35,41,44,49,50,54)

# Select the features using the select function and magrittr's pipe operator
df.sel <- df %>%
  select(selected_indexes)

# Reduce features from df
df.sel <- df %>% select(all_of(idx))

# Overview the resulting dataset
head(df.sel)

# Display the resulting Spark DataFrame df.sel
df.sel
head(df.sel)

################# G4 #######################

# Set the seed for reproducibility
set.seed(123)

# Define the split proportions
train_proportion <- 2 / 3
test_proportion <- 1 - train_proportion

# Split the dataset into training and testing sets
splits <- sdf_random_split(df.sel, training = train_proportion, test = test_proportion)

# Extract the training and testing sets
df.train <- splits[[1]]
df.test <- splits[[2]]

#Generating train and test data

# Split the dataframe into training (2/3) and testing (1/3) sets
df.split <- df.sel %>% sdf_random_split(seed = 123, training = 2/3, testing = 1/3)

# Assign training and testing dataframes
df_train <- df.split$training
df_test <- df.split$testing

#TODO Baseline
rf_model <- ml_random_forest(df_train, CLASS ~ ., type = "classification")

# Convert Spark DataFrames to R data frames
df.train_local <- collect(df.train)
df.test_local <- collect(df.test)

# Use the table function to determine the number of instances for each class in both datasets
class_distribution_train <- table(df.train_local$CLASS)
class_distribution_test <- table(df.test_local$CLASS)

# Display the class distributions
print("Class Distribution in Training Set:")
print(class_distribution_train)
print("Class Distribution in Testing Set:")
print(class_distribution_test)

# Define the formula for classification
formula <- "CLASS ~ ."

# Train a Random Forest classification model
rf_model <- ml_random_forest(df.train, formula)

# Print the trained model
print(rf_model)

# Make predictions on the test dataset
predictions <- mdle.predict(rf_model, df.test)

# Print confusion matrix and evaluate model performance
mdle.printConfusionMatrix(predictions, "")

################# G5 #######################
#Using imbalanced correcting sampling techniques
#df.pos.train<- #TODO
#df.neg.train<- #TODO
#TODO

#Oversampling
#TODO

#BSMOTE
#TODO

################# Spark cleanup ################
spark_disconnect(sc)
