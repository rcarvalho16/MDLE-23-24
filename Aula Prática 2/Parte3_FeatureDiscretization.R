#install.packages("classInt")
library(classInt)

data_lisbon <- read.csv("Lisbon_ 2023-01-01_2023-01-31.csv")

# Unsupervised Method
# Equal Frequency Binning

for(feature in colnames(data_lisbon)){
  classes <- sapply(data_lisbon[feature], class)
  
  # For each column divide in bins. The number of bins must not be bigger than the number of unique values
  if(any(c("numeric", "integer") %in% classes)){
    bins <- floor(sqrt(length(unique(data_lisbon[[feature]]))))
    if(bins > 1){
      classIntervals(data_lisbon[[feature]], bins, style = 'quantile')
    }
  }
}

# Supervised Method