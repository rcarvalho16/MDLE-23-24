################# Preparation ################
library(dplyr) #data manipulation
library(sparklyr) #spark 
library(smotefamily) #For SMOTE sampling
library(data.table) #To be used when possible, as a more performant data.frame

if(!exists("printConfusionMatrix", mode="function")) 
  source("helperfunctions.R")

################# Spark setup ################
spark_disconnect_all() #just preventive code
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
#Glimpse of the data set
# Get a glimpse of the first and last 10 rows of the dataset
head(df, n = 10)
# Function stopifnot() prints NULL if all statements are true
# Sparklyr's equivalent of nrow is sdf_nrow()
print(stopifnot(sdf_nrow(df) != 2190))


################# G3 #######################
#Feature Selection
idx <- c(1,2,5,6,9,10,11,14,16,17,19,21,24,25,26,31,32,33,34,35,41,44,49,50,54)

#TODO


################# G4 #######################
#Generating train and test data

#df.split <- #TODO
#df.train <- #TODO
#df.test <-  #TODO

#TODO Baseline

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
