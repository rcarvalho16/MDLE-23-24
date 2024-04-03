#install.packages("arulesCBA")
library(arulesCBA)

data_lisbon <- read.csv("Lisbon_ 2023-01-01_2023-01-31.csv")

# Unsupervised Method
# Equal Frequency Binning (EFB)

unsupervised_data_lisbon <- data_lisbon

for(feature in colnames(unsupervised_data_lisbon)){
  data <- unsupervised_data_lisbon[[feature]]
  if(class(data) %in% c("numeric", "integer")){
    n_bins <- floor(log(length(unique(data)), 2))
    if(n_bins == 0){
      next
    }
    bin_breaks <- quantile(data, probs = seq(0,1,1/n_bins)[-1])
    bin_breaks <- unique(bin_breaks)
    if(length(bin_breaks) <= 1){
      next
    }
    bin_breaks <- c(-Inf, bin_breaks)
    unsupervised_data_lisbon[feature] <- cut(data, breaks = bin_breaks)
  }
}

# Supervised Method
# Class-Attribute Interdependence Maximization (CAIM)

supervised_data_lisbon <- data_lisbon
supervised_data_lisbon$preciptype <- as.factor(supervised_data_lisbon$preciptype)

supervised_data_lisbon <- discretizeDF.supervised(preciptype ~ ., supervised_data_lisbon, "caim")

